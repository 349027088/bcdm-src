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
import com.bcdm.foodtraceability.service.EmpowerService;
import com.bcdm.foodtraceability.service.JurisdictionService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import static com.bcdm.foodtraceability.common.Constants.*;
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
@Slf4j
public class CompanyServiceImpl extends ServiceImpl<CompanyMapper, Company> implements CompanyService {

    private final JurisdictionService jurisdictionService;

    private final EmpowerService empowerService;

    public CompanyServiceImpl(JurisdictionService jurisdictionService, EmpowerService empowerService) {
        this.jurisdictionService = jurisdictionService;
        this.empowerService = empowerService;
    }

    @Override
    public Company register(User user, Company company) throws Exception {
        List<Company> companyList = jurisdictionGetCompanyList(user.getUserId());
        if (companyList.size() <= COMPANY_MAX) {
            createNewCompanyInfo(company);
            if (!save(company)) {
                throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_COMPANY_FAIL);
            }
            jurisdictionService.createJurisdiction(user.getUserId(), company.getCompanyId(), COMPANY_USER_0);
            empowerService.createCompanyServiceEmpower(company);
            return company;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_COMPANY_FAIL);
    }

    @Override
    public Company modify(Company company) throws Exception {
        log.info(company.toString());
        List<Company> companyList = jurisdictionGetCompanyList(company.getUserId());
        for (Company selectCompany : companyList) {
            if (selectCompany.getCompanyId().equals(company.getCompanyId())) {
                UpdateWrapper<Company> companyUpdateWrapper = new UpdateWrapper<>();
                LocalDateTime now = LocalDateTime.now();
                companyUpdateWrapper
                        .eq("company_id", company.getCompanyId())
                        .eq("update_time", company.getUpdateTime())
                        .eq("user_id", company.getUserId())
                        .set("company_icon", company.getCompanyIcon())
                        .set("company_name", company.getCompanyName())
                        .set("company_info", company.getCompanyInfo())
                        .set("company_address", company.getCompanyAddress())
                        .set("business_license", company.getBusinessLicense())
                        .set("health_permit", company.getHealthPermit())
                        .set("update_time", now);
                if (update(companyUpdateWrapper)) {
                    return getById(company.getCompanyId());
                }
            }
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_COMPANY_FAIL);
    }

    @Override
    public List<Company> getCompanyByUser(User user) throws Exception {
        log.info(user.getUserId() + "---------------" + "getCompanyByUser");
        List<Company> companyList = jurisdictionGetCompanyList(user.getUserId());
        if (SELECT_ZERO != companyList.size()) {
            return companyList;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, USER_GET_COMPANY_INFO_FAIL);
    }

    @Override
    public Company createUserToCompany(Integer user_id, Company company) throws Exception {
        if (null == company.getCompanyId()) {
            QueryWrapper<Company> companyQueryWrapper = new QueryWrapper<>();
            companyQueryWrapper.eq("company_name", company.getCompanyName());
            company = getOne(companyQueryWrapper);
        } else {
            company = getById(company.getCompanyId());
        }
        jurisdictionService.createJurisdiction(user_id, company.getCompanyId(), COMPANY_USER_3);
        return company;
    }

    /**
     * 新公司创建初始化
     *
     * @param company 需要被初始化信息的公司情报
     */
    private void createNewCompanyInfo(Company company) {
        LocalDateTime now = LocalDateTime.now();
        company.setCreateTime(now);
        company.setUpdateTime(now);
        company.setCompanyLevel(COMPANY_LEVEL_NORMAL);
        company.setCompanyStatus(COMPANY_STATUS_OUT_OF_SERVICE);
        log.info(company.toString());
    }

    /**
     * 获取用户关联的企业信息
     *
     * @param userId 需要查询相关企业信息的用户
     * @return 被查询的企业信息
     * @throws Exception 查询关联表信息失败
     */
    private List<Company> jurisdictionGetCompanyList(Integer userId) throws Exception {
        List<Jurisdiction> jurisdictionList = jurisdictionService.getJurisdictionByUser(userId);
        List<Company> companyList = new ArrayList<>();
        boolean isCompanyUser = true;
        for (Jurisdiction jurisdiction : jurisdictionList) {
            if (!COMPANY_USER_3.equals(jurisdiction.getJurisdiction())) {
                log.info(jurisdiction.toString());
                QueryWrapper<Company> companyQueryWrapper = new QueryWrapper<>();
                companyQueryWrapper.eq("company_id", jurisdiction.getCompanyId());
                Company company = getOne(companyQueryWrapper);
                companyList.add(company);
                isCompanyUser = true;
            } else {
                isCompanyUser = false;
            }
        }
        if (isCompanyUser) {
            return companyList;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, USER_ADMIT_FAIL);
    }


}
