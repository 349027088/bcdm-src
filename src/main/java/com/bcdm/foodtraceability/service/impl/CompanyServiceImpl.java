package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.Jurisdiction;
import com.bcdm.foodtraceability.entity.User;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.CompanyMapper;
import com.bcdm.foodtraceability.service.CompanyService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.bcdm.foodtraceability.service.JurisdictionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

import static com.bcdm.foodtraceability.common.Constants.SELECT_ZERO;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
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

    @Autowired
    private JurisdictionService jurisdictionService;

    @Override
    public Company register(User user, Company company) throws Exception {
        List<Jurisdiction> jurisdictionList = jurisdictionService.getJurisdiction(user);
        return null;
    }

    @Override
    public Company modify(User user, Company company) throws Exception {
        List<Jurisdiction> jurisdictionList = jurisdictionService.getJurisdiction(user);
        return null;
    }

    @Override
    public List<Company> getCompanyByUser(User user) throws Exception {
        List<Jurisdiction> jurisdictionList = jurisdictionService.getJurisdiction(user);
        List<Company> companyList = new ArrayList<>();
        for (Jurisdiction jurisdiction : jurisdictionList) {
            QueryWrapper<Company> companyQueryWrapper = new QueryWrapper<>();
            companyQueryWrapper.eq("company_id", jurisdiction.getCompanyId());
            Company company = getOne(companyQueryWrapper);
            companyList.add(company);
        }
        if (SELECT_ZERO!=companyList.size()){
            return companyList;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, USER_GET_COMPANY_INFO_FAIL);
    }



}
