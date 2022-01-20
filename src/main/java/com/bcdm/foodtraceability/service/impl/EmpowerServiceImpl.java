package com.bcdm.foodtraceability.service.impl;

import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.Empower;
import com.bcdm.foodtraceability.entity.Management;
import com.bcdm.foodtraceability.mapper.EmpowerMapper;
import com.bcdm.foodtraceability.service.EmpowerService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.util.List;

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
        //TODO
        return null;
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
}
