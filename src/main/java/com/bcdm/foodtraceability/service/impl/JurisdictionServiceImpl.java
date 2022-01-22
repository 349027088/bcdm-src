package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.bcdm.foodtraceability.entity.Jurisdiction;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.JurisdictionMapper;
import com.bcdm.foodtraceability.service.JurisdictionService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;

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
    public void createJurisdiction(Integer userId, Integer companyId, Integer identity) throws Exception {
        Jurisdiction jurisdiction = createJurisdictionEntity(userId, companyId, identity);
        if (!save(jurisdiction)) {
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
                    .set("update_time",LocalDateTime.now())
                    .set("jurisdiction", jurisdiction.getIdentity())
                    .set("identity", jurisdiction.getIdentity());
            if (!update(jurisdictionUpdateWrapper)) {
                throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_JURISDICTION_FAIL);
            }
        } else {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_JURISDICTION_LEVEL_FAIL);
        }
        return true;
    }

    private boolean jurisdictionCheck(Integer companyManagerUserId, Jurisdiction jurisdiction) {
        List<Jurisdiction> jurisdictionByUser = getJurisdictionByUser(companyManagerUserId);
        for (Jurisdiction jurisdictionEntity : jurisdictionByUser) {
            if (jurisdictionEntity.getCompanyId().equals(jurisdiction.getCompanyId())) {
                Jurisdiction compareJurisdiction =
                        getOne(new QueryWrapper<Jurisdiction>()
                        .eq("user_id",jurisdiction.getUserId())
                        .eq("company_id",jurisdiction.getCompanyId()));
                if (compareJurisdiction.getJurisdiction() > jurisdictionEntity.getJurisdiction()){
                    return true;
                }

            }
        }
        return false;
    }

    private Jurisdiction createJurisdictionEntity(Integer userId, Integer companyId, Integer identity) {
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
