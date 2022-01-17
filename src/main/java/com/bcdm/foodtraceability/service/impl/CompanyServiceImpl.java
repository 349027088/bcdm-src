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
import com.bcdm.foodtraceability.service.JurisdictionService;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;

import static com.bcdm.foodtraceability.common.Constants.COMPANY_MAX;
import static com.bcdm.foodtraceability.common.Constants.SELECT_ZERO;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.CREATE_COMPANY_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.USER_GET_COMPANY_INFO_FAIL;

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

    public CompanyServiceImpl(JurisdictionService jurisdictionService) {
        this.jurisdictionService = jurisdictionService;
    }

    @Override
    public Company register(User user, Company company) throws Exception {
        List<Company> companyList = jurisdictionService.getJurisdiction(user);
        if (companyList.size() <= COMPANY_MAX){
            LocalDateTime now = LocalDateTime.now();
            company.setCreateTime(now);
            company.setUpdateTime(now);
            if (!save(company)){
                throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_COMPANY_FAIL);
            }
            return company;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_COMPANY_FAIL);
    }

    @Override
    public Company modify(User user, Company company) throws Exception {
        List<Company> companyList = jurisdictionService.getJurisdiction(user);
        for (Company selectCompany:companyList) {
            if (Objects.equals(company.getCompanyId(), selectCompany.getCompanyId())){
                UpdateWrapper<Company> companyUpdateWrapper = new UpdateWrapper<>();
                companyUpdateWrapper
                        .eq("company_id",company.getCompanyId())
                        .eq("update_time",company.getUpdateTime())
                        .set("update_time",LocalDateTime.now());
                if (!update(companyUpdateWrapper)){
                    throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_COMPANY_FAIL);
                }
            }
        }
        return null;
    }

    @Override
    public List<Company> getCompanyByUser(User user) throws Exception {
        List<Company> companyList = jurisdictionService.getJurisdiction(user);
        if (SELECT_ZERO!=companyList.size()){
            return companyList;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, USER_GET_COMPANY_INFO_FAIL);
    }



}
