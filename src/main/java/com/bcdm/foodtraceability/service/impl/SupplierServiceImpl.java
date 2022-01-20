package com.bcdm.foodtraceability.service.impl;

import com.bcdm.foodtraceability.entity.Supplier;
import com.bcdm.foodtraceability.mapper.SupplierMapper;
import com.bcdm.foodtraceability.service.SupplierService;
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
public class SupplierServiceImpl extends ServiceImpl<SupplierMapper, Supplier> implements SupplierService {

    @Override
    public Supplier createSupplier(Supplier supplier) throws Exception {
        //TODO
        return null;
    }

    @Override
    public Supplier modifySupplier(Supplier supplier) throws Exception {
        //TODO
        return null;
    }

    @Override
    public List<Supplier> getSupplierList() throws Exception {
        //TODO
        return null;
    }
}
