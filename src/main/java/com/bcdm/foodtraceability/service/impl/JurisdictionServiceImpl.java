package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.bcdm.foodtraceability.entity.Jurisdiction;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.JurisdictionMapper;
import com.bcdm.foodtraceability.service.JurisdictionService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;

import static com.bcdm.foodtraceability.common.Constants.COMPANY_USER_99;
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
@Slf4j
public class JurisdictionServiceImpl extends ServiceImpl<JurisdictionMapper, Jurisdiction> implements JurisdictionService {

    @Override
    public Jurisdiction getJurisdictionByUser(Integer userId) {
        QueryWrapper<Jurisdiction> jurisdictionQueryWrapper = new QueryWrapper<>();
        jurisdictionQueryWrapper.eq("user_id", userId);
        return getOne(jurisdictionQueryWrapper);
    }

    @Override
    public List<Jurisdiction> getJurisdictionByCompany(Integer CompanyId) {
        QueryWrapper<Jurisdiction> jurisdictionQueryWrapper = new QueryWrapper<>();
        jurisdictionQueryWrapper.eq("company_id", CompanyId);
        return list(jurisdictionQueryWrapper);
    }

    @Override
    public void createJurisdiction(Integer userId, Integer companyId, Integer identity) throws Exception {
        if (!save(createJurisdictionEntity(userId, companyId, identity))) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_JURISDICTION_FAIL);
        }
    }

    @Override
    public Boolean modifyJurisdiction(Jurisdiction jurisdiction, Integer companyManagerUserId) throws Exception {
        if (jurisdictionCheck(companyManagerUserId, jurisdiction)) {
            UpdateWrapper<Jurisdiction> jurisdictionUpdateWrapper = new UpdateWrapper<>();
            jurisdictionUpdateWrapper
                    .eq("user_id", jurisdiction.getUserId())
                    .eq("company_id", jurisdiction.getCompanyId())
                    .eq("update_time", jurisdiction.getUpdateTime())
                    .set("update_time", LocalDateTime.now())
                    .set("jurisdiction", jurisdiction.getJurisdiction())
                    .set("identity", jurisdiction.getIdentity());
            if (update(jurisdictionUpdateWrapper)) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_JURISDICTION_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_JURISDICTION_LEVEL_FAIL);
    }

    @Override
    public Boolean deleteJurisdiction(Jurisdiction jurisdiction, Integer companyManagerUserId) throws Exception {
        if (jurisdictionCheck(companyManagerUserId, jurisdiction)) {
            QueryWrapper<Jurisdiction> jurisdictionUpdateWrapper = new QueryWrapper<>();
            jurisdictionUpdateWrapper
                    .eq("user_id", jurisdiction.getUserId())
                    .eq("company_id", jurisdiction.getCompanyId())
                    .eq("update_time", jurisdiction.getUpdateTime());
            if (remove(jurisdictionUpdateWrapper)) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, DELETE_JURISDICTION_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, DELETE_JURISDICTION_LEVEL_FAIL);
    }

    private boolean jurisdictionCheck(Integer companyManagerUserId, Jurisdiction jurisdiction) {
        Jurisdiction managementJurisdiction = getJurisdictionByUser(companyManagerUserId);
        if (managementJurisdiction.getCompanyId().equals(jurisdiction.getCompanyId())) {
            Jurisdiction compareJurisdiction =
                    getOne(new QueryWrapper<Jurisdiction>()
                            .eq("user_id", jurisdiction.getUserId())
                            .eq("company_id", jurisdiction.getCompanyId()));
            return null != compareJurisdiction && compareJurisdiction.getJurisdiction() > managementJurisdiction.getJurisdiction() &&
                    jurisdiction.getJurisdiction() > managementJurisdiction.getJurisdiction();
        }
        return false;
    }

    private Jurisdiction createJurisdictionEntity(Integer userId, Integer companyId, Integer identity) throws Exception {
        if (SELECT_ZERO == count(new QueryWrapper<Jurisdiction>().eq("user_id", userId))) {
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
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_JURISDICTION_FAIL);
    }

}
