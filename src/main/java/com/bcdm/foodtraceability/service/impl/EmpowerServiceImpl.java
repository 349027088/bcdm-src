package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.entity.*;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.EmpowerMapper;
import com.bcdm.foodtraceability.service.EmpowerService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;

import static com.bcdm.foodtraceability.common.Constants.*;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 授权服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Service
public class EmpowerServiceImpl extends ServiceImpl<EmpowerMapper, Empower> implements EmpowerService {

    @Override
    public void createCompanyServiceEmpower(Company company) throws Exception {
        Empower newEmpower = createEmpowerEntity(company.getCompanyId());
        if (!save(newEmpower)) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_EMPOWER_FAIL);
        }
    }

    @Override
    public Empower createNewEmpower(Empower empower) throws Exception {
        LocalDateTime now = LocalDateTime.now();
        empower.setStartDate(empower.getStartDate());
        empower.setServiceStopTime(now.plusDays(empower.getUsageTime()).toLocalDate());
        empower.setCreateTime(now);
        empower.setUpdateTime(now);
        if (!save(empower)) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, ENTERPRISE_AUTHORIZATION_FAILED);
        }
        return empower;
    }

    @Override
    public Empower modifyEmpower(Empower empower) throws Exception {
        LocalDateTime now = LocalDateTime.now();
        Empower selectEmpower = getById(empower.getEmpowerId());
        if (null != selectEmpower) {
            UpdateWrapper<Empower> updateWrapper = new UpdateWrapper<>();
            updateWrapper
                    .eq("empowerId",empower.getEmpowerId())
                    .eq("update_time", empower.getUpdateTime())
                    .set("manager_id", empower.getManagerId())
                    .set("company_id", empower.getCompanyId())
                    .set("money_amt", empower.getMoneyAmt())
                    .set("usage_time", empower.getUsageTime())
                    .set("service_stop_time", empower.getServiceStopTime())
                    .set("authorization_certificate", empower.getAuthorizationCertificate())
                    .set("update_time", now);
            if (update(updateWrapper)) {
                return empower;
            }
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_EMPOWER_FAIL);
    }

    @Override
    public IPage<Empower> getEmpowerList(SelectPageEntity<Empower> selectInfo) throws Exception {
        QueryWrapper<Empower> manufacturerQueryWrapper = new QueryWrapper<>();
        if (!StringUtils.isEmpty(selectInfo.getSelectName())) {
            manufacturerQueryWrapper.likeRight("manufacturer_name", selectInfo.getSelectName());
        }
        manufacturerQueryWrapper.eq("company_id", selectInfo.getCompanyId());
        selectInfo.setPageInfo(page(selectInfo.getPageInfo(), manufacturerQueryWrapper));
        if (SELECT_ZERO == selectInfo.getPageInfo().getTotal()) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, SELECT_EMPOWER_FAIL);
        }
        return selectInfo.getPageInfo();
    }

    /**
     * 创建授权信息
     *
     * @param companyId 需要新授权的公司ID
     * @return 创建的授权信息
     */
    private Empower createEmpowerEntity(int companyId) {
        LocalDateTime now = LocalDateTime.now();
        Empower newEmpower = new Empower();
        newEmpower.setManagerId(DEFAULT_MANAGEMENT_ID);
        newEmpower.setStartDate(now.toLocalDate());
        newEmpower.setCompanyId(companyId);
        newEmpower.setCreateTime(now);
        newEmpower.setUpdateTime(now);
        newEmpower.setMoneyAmt(DEFAULT_MONEY_AMT);
        newEmpower.setUsageTime(DEFAULT_USAGE_TIME);
        newEmpower.setServiceStopTime(now.plusDays(DEFAULT_USAGE_TIME).toLocalDate());
        return newEmpower;
    }
}
