package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.Jurisdiction;
import com.bcdm.foodtraceability.entity.User;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.JurisdictionMapper;
import com.bcdm.foodtraceability.service.CompanyService;
import com.bcdm.foodtraceability.service.JurisdictionService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.bcdm.foodtraceability.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.CREATE_JURISDICTION_FAIL;

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

    @Override
    public List<Jurisdiction> getJurisdictionByUser(Integer userId) {
        QueryWrapper<Jurisdiction> jurisdictionQueryWrapper = new QueryWrapper<>();
        jurisdictionQueryWrapper.eq("user_id", userId);
        return list(jurisdictionQueryWrapper);
    }

    @Override
    public List<Jurisdiction> getJurisdictionByCompany(Integer CompanyId) {
        QueryWrapper<Jurisdiction> jurisdictionQueryWrapper = new QueryWrapper<>();
        jurisdictionQueryWrapper.eq("company_id", CompanyId);
        return list(jurisdictionQueryWrapper);
    }

    @Override
    public void createJurisdiction(Integer userId, Integer companyId,Integer identity) throws Exception {
        Jurisdiction jurisdiction = createJurisdictionEntity(userId,companyId,identity);
        if (!save(jurisdiction)){
            throw new ServiceBusinessException(HTTP_RETURN_FAIL,CREATE_JURISDICTION_FAIL);
        }
    }

    @Override
    public boolean modifyJurisdiction(Integer userId, Integer companyId, Integer identity) throws Exception {
        UpdateWrapper<Jurisdiction> jurisdictionUpdateWrapper = new UpdateWrapper<>();
        jurisdictionUpdateWrapper
                .eq("company_id",companyId)
                .eq("user_id",userId)
                .set("identity",identity);
        if (!update(jurisdictionUpdateWrapper)){
            throw new ServiceBusinessException(HTTP_RETURN_FAIL,CREATE_JURISDICTION_FAIL);
        }
        QueryWrapper<Jurisdiction> jurisdictionQueryWrapper = new QueryWrapper<>();
        jurisdictionQueryWrapper
                .eq("company_id",companyId)
                .eq("user_id",userId);
        return true;
    }

    private Jurisdiction createJurisdictionEntity(Integer userId,Integer companyId,Integer identity) {
        LocalDateTime now = LocalDateTime.now();
        Jurisdiction jurisdiction = new Jurisdiction();
        jurisdiction.setUserId(userId);
        jurisdiction.setCompanyId(companyId);
        jurisdiction.setIdentity(identity);
        jurisdiction.setJurisdiction(identity);
        jurisdiction.setUpdateTime(now);
        jurisdiction.setCreateTime(now);
        return jurisdiction;
    }

}
