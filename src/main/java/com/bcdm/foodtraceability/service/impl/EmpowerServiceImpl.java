package com.bcdm.foodtraceability.service.impl;

import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.Empower;
import com.bcdm.foodtraceability.entity.Management;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.EmpowerMapper;
import com.bcdm.foodtraceability.service.EmpowerService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;

import static com.bcdm.foodtraceability.common.Constants.*;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.CREATE_EMPOWER_FAIL;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Service
public class EmpowerServiceImpl extends ServiceImpl<EmpowerMapper, Empower> implements EmpowerService {

    @Override
    public Empower createCompanyServiceEmpower(Company company) throws Exception {
        Empower newEmpower = createEmpowerEntity(company.getCompanyId());
        if (!save(newEmpower)){
            throw new ServiceBusinessException(HTTP_RETURN_FAIL,CREATE_EMPOWER_FAIL);
        }
        return newEmpower;
    }

    @Override
    public Empower createNewEmpower(Management management, Empower empower) throws Exception {
        //TODO
        return null;
    }

    @Override
    public Empower modifyEmpower(Management management, Empower empower) throws Exception {
        //TODO
        return null;
    }

    @Override
    public List<Empower> getEmpowerList(Management management) throws Exception {
        //TODO
        return null;
    }

    /**
     * 新规企业创建时候附带的授权信息
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
        newEmpower.setSevericeStopTime(LocalDate.from(now.plus(1, ChronoUnit.MONTHS)));
        return newEmpower;
    }
}
