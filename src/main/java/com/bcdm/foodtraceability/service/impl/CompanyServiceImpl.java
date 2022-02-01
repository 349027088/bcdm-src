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
import com.bcdm.foodtraceability.service.IconService;
import com.bcdm.foodtraceability.service.JurisdictionService;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

import static com.bcdm.foodtraceability.common.Constants.*;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_LOGIN_WAIT;
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

    public CompanyServiceImpl(JurisdictionService jurisdictionService, EmpowerService empowerService, IconService iconService) {
        this.jurisdictionService = jurisdictionService;
        this.empowerService = empowerService;
        this.iconService = iconService;
    }

    @Override
    public Company register(Company company) throws Exception {
        Jurisdiction jurisdiction = jurisdictionService.getJurisdictionByUser(company.getUserId());
        if (null != jurisdiction) {
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
            iconDelete(company);
            if (modifyCompanyInfo(company)) {
                return getById(company.getCompanyId());
            }
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_COMPANY_FAIL);
    }

    @Override
    public Company getCompanyByUser(User user) throws Exception {
        Company company = jurisdictionGetCompany(user.getUserId());
        if (null != company) {
            return company;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, USER_GET_COMPANY_INFO_FAIL);
    }

    @Override
    public Company createUserToCompany(Company company) throws Exception {
        if (null == company.getCompanyId()) {
            QueryWrapper<Company> companyQueryWrapper = new QueryWrapper<>();
            companyQueryWrapper.eq("company_name", company.getCompanyName());
            company = getOne(companyQueryWrapper);
        } else {
            company = getById(company.getCompanyId());
        }
        jurisdictionService.createJurisdiction(company.getUserId(), company.getCompanyId(), COMPANY_USER_3);
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
    private Company jurisdictionGetCompany(Integer userId) throws Exception {
        Jurisdiction jurisdiction = jurisdictionService.getJurisdictionByUser(userId);
        //如果用户为非申请中的员工返回公司信息
        if (!COMPANY_USER_3.equals(jurisdiction.getJurisdiction())) {
            QueryWrapper<Company> companyQueryWrapper = new QueryWrapper<>();
            companyQueryWrapper.eq("company_id", jurisdiction.getCompanyId());
            return getOne(companyQueryWrapper);
        } else {
            throw new ServiceBusinessException(HTTP_LOGIN_WAIT, USER_ADMIT_FAIL);
        }
    }

    /**
     * 更新信息包含图片的时候删除旧图片释放七牛云空间
     *
     * @param company 需要更新的公司信息
     * @throws Exception 删除图片发生异常
     */
    private void iconDelete(Company company) throws Exception {
        Company compareCompany = getById(company.getCompanyId());
        if (null == compareCompany.getCompanyIcon() || !compareCompany.getCompanyIcon().equals(company.getCompanyIcon())){
            iconService.deleteIcon(compareCompany.getCompanyIcon());
        }
        if (null == compareCompany.getBusinessLicense() || !compareCompany.getBusinessLicense().equals(company.getBusinessLicense())){
            iconService.deleteIcon(compareCompany.getBusinessLicense());
        }
        if (null == compareCompany.getHealthPermit() || !compareCompany.getHealthPermit().equals(company.getHealthPermit())){
            iconService.deleteIcon(compareCompany.getHealthPermit());
        }
    }

    /**
     * 修改公司信息SQL
     *
     * @param company 需要修改的公司信息
     * @return 修改信息结果
     */
    private boolean modifyCompanyInfo(Company company) {
        UpdateWrapper<Company> companyUpdateWrapper = new UpdateWrapper<>();
        LocalDateTime now = LocalDateTime.now();
        companyUpdateWrapper
                .eq("company_id", company.getCompanyId())
                .eq("update_time", company.getUpdateTime())
                .eq("user_id", company.getUserId())
                .set("company_phone", company.getCompanyPhone())
                .set("company_icon", company.getCompanyIcon())
                .set("company_info", company.getCompanyInfo())
                .set("company_address", company.getCompanyAddress())
                .set("update_time", now);
        return update(companyUpdateWrapper);
    }
}