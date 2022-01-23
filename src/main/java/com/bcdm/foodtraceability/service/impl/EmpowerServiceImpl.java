package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.Empower;
import com.bcdm.foodtraceability.entity.Management;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.EmpowerMapper;
import com.bcdm.foodtraceability.service.EmpowerService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
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
    public List<Empower> getEmpowerList(Management management) throws Exception {
        List<Empower> empowerList = list();
        if (SELECT_ZERO != empowerList.size()) {
            return empowerList;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, INQUIRE_EMPOWER_FAIL);
    }

    /**
     * 新规企业创建时候附带的授权信息
     *
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
