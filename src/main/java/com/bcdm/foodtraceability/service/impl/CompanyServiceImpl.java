package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.entity.*;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.CompanyMapper;
import com.bcdm.foodtraceability.service.*;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;

import static com.bcdm.foodtraceability.common.Constants.*;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 公司服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Service
public class CompanyServiceImpl extends ServiceImpl<CompanyMapper, Company> implements CompanyService {

    private final JurisdictionService jurisdictionService;

    private final EmpowerService empowerService;

    private final IconService iconService;

    private final ManagementService managementService;

    public CompanyServiceImpl(JurisdictionService jurisdictionService, EmpowerService empowerService, IconService iconService, ManagementService managementService) {
        this.jurisdictionService = jurisdictionService;
        this.empowerService = empowerService;
        this.iconService = iconService;
        this.managementService = managementService;
    }

    @Override
    public IPage<Company> getCompanyList(SelectPageEntity<Company> selectPageEntity) throws Exception {
        QueryWrapper<Company> companyQueryWrapper = new QueryWrapper<>();
        if (!StringUtils.isEmpty(selectPageEntity.getSelectName())) {
            companyQueryWrapper.likeRight("company_name", selectPageEntity.getSelectName());
            selectPageEntity.setPageInfo(page(selectPageEntity.getPageInfo(), companyQueryWrapper));
        } else {
            selectPageEntity.setPageInfo(page(selectPageEntity.getPageInfo()));
        }
        if (SELECT_ZERO != selectPageEntity.getPageInfo().getTotal()) {
            return selectPageEntity.getPageInfo();
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, GET_COMPANY_LIST_FAIL);
    }

    @Override
    public Company register(Company company) throws Exception {
        Jurisdiction jurisdiction = jurisdictionService.getJurisdictionByUser(company.getUserId());
        if (null == jurisdiction) {
            createNewCompanyInfo(company);
            if (!save(company)) {
                throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_COMPANY_FAIL);
            }
            jurisdictionService.createJurisdiction(company.getUserId(), company.getCompanyId(), COMPANY_USER_0);
            empowerService.createCompanyServiceEmpower(company);
            return company;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_COMPANY_FAIL);
    }

    @Override
    public Company modify(Company company) throws Exception {
        Jurisdiction jurisdiction = jurisdictionService.getJurisdictionByUser(company.getUserId());
        if (COMPANY_USER_0.equals(jurisdiction.getJurisdiction()) || COMPANY_USER_1.equals(jurisdiction.getJurisdiction())) {
            Company compareCompany = getById(company.getCompanyId());
            iconDelete(company, compareCompany);
            return modifyCompanyInfo(company);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_COMPANY_FAIL);
    }

    @Override
    public Boolean modifyBusinessLicense(String managementId, Company company) throws Exception {
        managementService.checkManager(managementId);
        UpdateWrapper<Company> companyUpdateWrapper = new UpdateWrapper<>();
        companyUpdateWrapper
                .eq("company_id", company.getCompanyId())
                .set("update_time", LocalDateTime.now())
                .set("business_license", company.getBusinessLicense());
        if (update(companyUpdateWrapper)) {
            return true;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_COMPANY_FAIL);
    }

    @Override
    public Boolean modifyHealthPermit(String managementId, Company company) throws Exception {
        managementService.checkManager(managementId);
        UpdateWrapper<Company> companyUpdateWrapper = new UpdateWrapper<>();
        companyUpdateWrapper
                .eq("company_id", company.getCompanyId())
                .set("update_time", LocalDateTime.now())
                .set("health_permit", company.getHealthPermit());
        if (update(companyUpdateWrapper)) {
            return true;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_COMPANY_FAIL);
    }

    @Override
    public Company getCompanyInfo(Company company) throws Exception {
        Company selectCompany = getById(company.getCompanyId());
        if (null != selectCompany && COMPANY_STATUS_ON_SERVICE.equals(selectCompany.getCompanyStatus())) {
            return selectCompany;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, GET_COMPANY_INFO_FAIL);
    }

    @Override
    public Map<String, Object> getCompanyByUser(User user) throws Exception {
        Map<String, Object> companyInfoMap = jurisdictionGetCompany(user.getUserId());
        if (null != companyInfoMap && null != companyInfoMap.get(COMPANY)) {
            if (COMPANY_STATUS_OUT_OF_SERVICE.equals(((Company) companyInfoMap.get(COMPANY)).getCompanyStatus())) {
                throw new ServiceBusinessException(HTTP_RETURN_FAIL, USER_GET_COMPANY_INFO_FAIL2);
            } else if (COMPANY_STATUS_IS_LOCK.equals(((Company) companyInfoMap.get(COMPANY)).getCompanyStatus())) {
                throw new ServiceBusinessException(HTTP_RETURN_FAIL, USER_GET_COMPANY_INFO_FAIL3);
            } else if (COMPANY_STATUS_TIME_STOP.equals(((Company) companyInfoMap.get(COMPANY)).getCompanyStatus())) {
                throw new ServiceBusinessException(HTTP_RETURN_FAIL, USER_GET_COMPANY_INFO_FAIL4);
            }
        }
        return companyInfoMap;
    }

    @Override
    public Company createUserToCompany(Company company) throws Exception {
        Integer userId = company.getUserId();
        if (null == company.getCompanyId()) {
            QueryWrapper<Company> companyQueryWrapper = new QueryWrapper<>();
            companyQueryWrapper.eq("company_name", company.getCompanyName());
            company = getOne(companyQueryWrapper);
        } else {
            company = getById(company.getCompanyId());
        }
        jurisdictionService.createJurisdiction(userId, company.getCompanyId(), COMPANY_USER_3);
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
    }

    /**
     * 获取用户关联的企业信息
     *
     * @param userId 需要查询相关企业信息的用户
     * @return 被查询的企业信息
     * @throws Exception 查询关联表信息失败
     */
    private Map<String, Object> jurisdictionGetCompany(Integer userId) throws Exception {
        Jurisdiction jurisdiction = jurisdictionService.getJurisdictionByUser(userId);
        //如果用户为非申请中的员工返回公司信息
        if (null != jurisdiction && !COMPANY_USER_3.equals(jurisdiction.getJurisdiction())) {
            QueryWrapper<Company> companyQueryWrapper = new QueryWrapper<>();
            companyQueryWrapper.eq("company_id", jurisdiction.getCompanyId());
            Map<String, Object> companyInfoMap = new HashMap<>();
            companyInfoMap.put(COMPANY, getOne(companyQueryWrapper));
            companyInfoMap.put(JURISDICTION, jurisdiction);
            return companyInfoMap;
        } else if (null != jurisdiction && COMPANY_USER_3.equals(jurisdiction.getJurisdiction())) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, USER_ADMIT_FAIL);
        } else {
            return null;
        }
    }

    /**
     * 更新信息包含图片的时候删除旧图片释放七牛云空间
     *
     * @param company 需要更新的公司信息
     * @throws Exception 删除图片发生异常
     */
    private void iconDelete(Company company, Company compareCompany) throws Exception {
        //企业LOGO更新验证
        if (!StringUtils.isEmpty(company.getCompanyIcon())
                && !StringUtils.isEmpty(compareCompany.getCompanyIcon())
                && !company.getCompanyIcon().equals(compareCompany.getCompanyIcon())) {
            iconService.deleteIcon(compareCompany.getCompanyIcon());
        }
        //企业营业执照更新验证
        if (null != company.getBusinessLicense()
                && null != compareCompany.getBusinessLicense()
                && !company.getBusinessLicense().equals(compareCompany.getBusinessLicense())) {
            iconService.deleteIcon(compareCompany.getBusinessLicense());
        }
        //企业经营执照更新验证
        if (null != company.getHealthPermit()
                && null != compareCompany.getHealthPermit()
                && !company.getHealthPermit().equals(compareCompany.getHealthPermit())) {
            iconService.deleteIcon(compareCompany.getHealthPermit());
        }
    }

    /**
     * 修改公司信息SQL
     *
     * @param company 需要修改的公司信息
     * @return 修改信息结果
     */
    private Company modifyCompanyInfo(Company company) throws Exception {
        UpdateWrapper<Company> companyUpdateWrapper = new UpdateWrapper<>();
        companyUpdateWrapper
                .eq("company_id", company.getCompanyId())
                .eq("update_time", company.getUpdateTime())
                .set("update_time", LocalDateTime.now())
                .set("company_phone", company.getCompanyPhone())
                .set("company_icon", company.getCompanyIcon())
                .set("company_info", company.getCompanyInfo())
                .set("company_address", company.getCompanyAddress());
        if (update(companyUpdateWrapper)) {
            return getById(company.getCompanyId());
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_COMPANY_FAIL);
    }
}