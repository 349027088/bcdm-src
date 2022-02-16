package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.bcdm.foodtraceability.common.FileUtil;
import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.CompanyInfoCheck;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.CompanyInfoCheckMapper;
import com.bcdm.foodtraceability.service.CompanyInfoCheckService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.Objects;

import static com.bcdm.foodtraceability.common.Constants.CUT_POINT;
import static com.bcdm.foodtraceability.common.Constants.SELECT_ZERO;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 企业经营信息变更服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-02-14
 */
@Service
public class CompanyInfoCheckServiceImpl extends ServiceImpl<CompanyInfoCheckMapper, CompanyInfoCheck> implements CompanyInfoCheckService {

    @Override
    public Boolean modifyBusinessLicense(CompanyInfoCheck company) throws Exception {
        int dotPos = Objects.requireNonNull(company.getBusinessLicense()).lastIndexOf(CUT_POINT);
        if (dotPos < 0) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, ICON_TYPE_FORMAT_FAIL);
        }
        if (!FileUtil.isFileAllowed(company.getBusinessLicense().substring(dotPos + 1).toLowerCase())) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, UPLOAD_FILE_IS_NOT_ICON);
        }
        if (SELECT_ZERO == count(new QueryWrapper<CompanyInfoCheck>().eq("company_id", company.getCompanyId()))) {
            LocalDateTime now = LocalDateTime.now();
            company.setCreateTime(now);
            company.setUpdateTime(now);
            if (save(company)) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_COMPANY_BUSINESS_LICENSE_FAIL);
        } else {
            CompanyInfoCheck compareCompanyInfo = getOne(new QueryWrapper<CompanyInfoCheck>().eq("company_id", company.getCompanyId()));
            if (StringUtils.isEmpty(compareCompanyInfo.getBusinessLicense())) {
                compareCompanyInfo.setBusinessLicense(company.getBusinessLicense());
                compareCompanyInfo.setUpdateTime(LocalDateTime.now());
                if (saveOrUpdate(compareCompanyInfo)) {
                    return true;
                }
                throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_COMPANY_BUSINESS_LICENSE_FAIL);
            }
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_COMPANY_IS_FAIL);
    }

    @Override
    public Boolean modifyHealthPermit(CompanyInfoCheck company) throws Exception {
        int dotPos = Objects.requireNonNull(company.getHealthPermit()).lastIndexOf(CUT_POINT);
        if (dotPos < 0) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, ICON_TYPE_FORMAT_FAIL);
        }
        if (!FileUtil.isFileAllowed(company.getHealthPermit().substring(dotPos + 1).toLowerCase())) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, UPLOAD_FILE_IS_NOT_ICON);
        }
        if (SELECT_ZERO == count(new QueryWrapper<CompanyInfoCheck>().eq("company_id", company.getCompanyId()))) {
            LocalDateTime now = LocalDateTime.now();
            company.setCreateTime(now);
            company.setUpdateTime(now);
            if (save(company)) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_COMPANY_HEALTH_PERMIT_FAIL);
        } else {
            CompanyInfoCheck compareCompanyInfo = getOne(new QueryWrapper<CompanyInfoCheck>().eq("company_id", company.getCompanyId()));
            if (StringUtils.isEmpty(compareCompanyInfo.getHealthPermit())) {
                compareCompanyInfo.setHealthPermit(company.getHealthPermit());
                compareCompanyInfo.setUpdateTime(LocalDateTime.now());
                if (saveOrUpdate(compareCompanyInfo)) {
                    return true;
                }

                throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_COMPANY_HEALTH_PERMIT_FAIL);
            }
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_COMPANY_IS_FAIL);
    }

    @Override
    public void deleteCheckInfo(Company company) throws Exception {
        if (remove(new QueryWrapper<CompanyInfoCheck>().eq("company_id",company.getCompanyId()))){
            return;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_COMPANY_FAIL);
    }

}
