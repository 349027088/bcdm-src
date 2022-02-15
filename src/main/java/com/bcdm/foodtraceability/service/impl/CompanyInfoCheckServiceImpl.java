package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.bcdm.foodtraceability.entity.CompanyInfoCheck;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.CompanyInfoCheckMapper;
import com.bcdm.foodtraceability.service.CompanyInfoCheckService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

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
        if (SELECT_ZERO == count(new QueryWrapper<CompanyInfoCheck>().eq("company_id", company.getCompanyId()))) {
            if (save(company)) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_COMPANY_BUSINESS_LICENSE_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_COMPANY_IS_FAIL);
    }

    @Override
    public Boolean modifyHealthPermit(CompanyInfoCheck company) throws Exception {
        if (SELECT_ZERO == count(new QueryWrapper<CompanyInfoCheck>().eq("company_id", company.getCompanyId()))) {
            if (save(company)) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_COMPANY_HEALTH_PERMIT_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_COMPANY_IS_FAIL);
    }

}
