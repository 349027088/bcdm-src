package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.Jurisdiction;
import com.bcdm.foodtraceability.entity.User;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.CompanyMapper;
import com.bcdm.foodtraceability.service.CompanyService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.bcdm.foodtraceability.service.IconService;
import com.bcdm.foodtraceability.service.JurisdictionService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static com.bcdm.foodtraceability.common.Constants.COMPANY_MAX;
import static com.bcdm.foodtraceability.common.Constants.SELECT_ZERO;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Service
public class CompanyServiceImpl extends ServiceImpl<CompanyMapper, Company> implements CompanyService {

    private final JurisdictionService jurisdictionService;

    private final IconService iconService;

    public CompanyServiceImpl(JurisdictionService jurisdictionService, IconService iconService) {
        this.jurisdictionService = jurisdictionService;
        this.iconService = iconService;
    }

    @Override
    public Company register(User user, Company company) throws Exception {
        List<Company> companyList = jurisdictionGetCompanyList(user);
        if (companyList.size() <= COMPANY_MAX) {
            LocalDateTime now = LocalDateTime.now();
            company.setCreateTime(now);
            company.setUpdateTime(now);
            if (!save(company)) {
                throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_COMPANY_FAIL);
            }
            return company;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_COMPANY_FAIL);
    }

    @Override
    public Company modify(User user, Company company) throws Exception {
        List<Company> companyList = jurisdictionGetCompanyList(user);
        for (Company selectCompany : companyList) {
            if (Objects.equals(company.getCompanyId(), selectCompany.getCompanyId())) {
                UpdateWrapper<Company> companyUpdateWrapper = new UpdateWrapper<>();
                companyUpdateWrapper
                        .eq("company_id", company.getCompanyId())
                        .eq("update_time", company.getUpdateTime())
                        .set("update_time", LocalDateTime.now());
                if (!update(companyUpdateWrapper)) {
                    throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_COMPANY_FAIL);
                }
            }
        }
        return null;
    }

    @Override
    public List<Company> getCompanyByUser(User user) throws Exception {
        List<Company> companyList = jurisdictionGetCompanyList(user);
        if (SELECT_ZERO != companyList.size()) {
            return companyList;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, USER_GET_COMPANY_INFO_FAIL);
    }

    @Override
    public String modifyCompanyIcon(MultipartFile file, Company company) throws Exception {
        Company selectCompany = getById(company.getCompanyId());
        if (null == selectCompany){
            throw new ServiceBusinessException(HTTP_RETURN_FAIL,FIND_COMPANY_BY_CREATE_ICON_FAIL);
        }
        if (!StringUtils.isEmpty(company.getCompanyIcon())){
            iconService.deleteIcon(company.getCompanyIcon());
        }
        String iconLink = iconService.createIcon(file);
        UpdateWrapper<Company> companyUpdateWrapper = new UpdateWrapper<>();
        companyUpdateWrapper
                .eq("company_id",selectCompany.getCompanyId())
                .eq("update_time",selectCompany.getUpdateTime())
                .set("update_time",LocalDateTime.now())
                .set("company_icon",iconLink);
        if (!update(companyUpdateWrapper)){
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, USER_GET_COMPANY_INFO_FAIL);
        }
        return iconLink;
    }

    @Override
    public String createCompanyIcon(MultipartFile file, Company company) throws Exception {
        return null;
    }

    private List<Company> jurisdictionGetCompanyList(User user) throws Exception {
        List<Jurisdiction> jurisdictionList = jurisdictionService.getJurisdictionByUser(user);
        List<Company> companyList = new ArrayList<>();
        for (Jurisdiction jurisdiction : jurisdictionList) {
            QueryWrapper<Company> companyQueryWrapper = new QueryWrapper<>();
            companyQueryWrapper.eq("company_id", jurisdiction.getCompanyId());
            Company company = getOne(companyQueryWrapper);
            companyList.add(company);
        }
        return companyList;
    }


}
