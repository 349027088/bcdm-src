package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.Jurisdiction;
import com.bcdm.foodtraceability.entity.User;
import com.bcdm.foodtraceability.mapper.JurisdictionMapper;
import com.bcdm.foodtraceability.service.CompanyService;
import com.bcdm.foodtraceability.service.JurisdictionService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.bcdm.foodtraceability.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Service
public class JurisdictionServiceImpl extends ServiceImpl<JurisdictionMapper, Jurisdiction> implements JurisdictionService {

    private final CompanyService companyService;

    private final UserService userService;

    public JurisdictionServiceImpl(CompanyService companyService, UserService userService) {
        this.companyService = companyService;
        this.userService = userService;
    }

    @Override
    public List<Company> getJurisdiction(User user) {
        QueryWrapper<Jurisdiction> jurisdictionQueryWrapper = new QueryWrapper<>();
        jurisdictionQueryWrapper.eq("user_id", user.getUserId());
        List<Jurisdiction> jurisdictionList = list(jurisdictionQueryWrapper);
        List<Company> companyList = new ArrayList<>();
        for (Jurisdiction jurisdiction : jurisdictionList) {
            QueryWrapper<Company> companyQueryWrapper = new QueryWrapper<>();
            companyQueryWrapper.eq("company_id", jurisdiction.getCompanyId());
            Company company = companyService.getOne(companyQueryWrapper);
            companyList.add(company);
        }
        return companyList;
    }

    @Override
    public List<User> getJurisdiction(Company company) {
        QueryWrapper<Jurisdiction> jurisdictionQueryWrapper = new QueryWrapper<>();
        jurisdictionQueryWrapper.eq("company_id", company.getCompanyId());
        List<Jurisdiction> jurisdictionList = list(jurisdictionQueryWrapper);

        List<User> userList = new ArrayList<>();
        for (Jurisdiction jurisdiction : jurisdictionList) {
            QueryWrapper<User> userQueryWrapper = new QueryWrapper<>();
            userQueryWrapper.eq("user_id", jurisdiction.getUserId());
            User user = userService.getOne(userQueryWrapper);
            userList.add(user);
        }
        return userList;
    }

}
