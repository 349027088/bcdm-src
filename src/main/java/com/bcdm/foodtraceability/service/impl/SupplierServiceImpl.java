package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.Supplier;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.SupplierMapper;
import com.bcdm.foodtraceability.service.SupplierService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;

import static com.bcdm.foodtraceability.common.Constants.*;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 供应商服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Service
public class SupplierServiceImpl extends ServiceImpl<SupplierMapper, Supplier> implements SupplierService {

    @Override
    public Boolean createSupplier(Supplier supplier) throws Exception {
        LocalDateTime now = LocalDateTime.now();
        supplier.setSupplierStatus(SUPPLIER_STATUS_ON_SERVICE);
        supplier.setSupplierLevel(SUPPLIER_LEVEL_ON_SERVICE);
        supplier.setCreateTime(now);
        supplier.setUpdateTime(now);
        if (!save(supplier)) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_SUPPLIER_FAILED);
        }
        return true;
    }

    @Override
    public Boolean modifySupplier(Supplier supplier) throws Exception {
        LocalDateTime now = LocalDateTime.now();
        Supplier selectSupplier = getById(supplier.getSupplierId());
        if (null != selectSupplier) {
            UpdateWrapper<Supplier> updateWrapper = new UpdateWrapper<>();
            updateWrapper
                    .eq("supplier_id", supplier.getSupplierId())
                    .eq("update_time", supplier.getUpdateTime())
                    .set("supplier_phone_number", supplier.getSupplierPhoneNumber())
                    .set("main_business", supplier.getMainBusiness())
                    .set("business_license", supplier.getBusinessLicense())
                    .set("health_permit", supplier.getHealthPermit())
                    .set("update_time", now);
            if (update(updateWrapper)) {
                return true;
            }
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_SUPPLIER_FAILED);
    }

    @Override
    public List<Supplier> getSupplierList(Company company) throws Exception {
        QueryWrapper<Supplier> queryWrapper = new QueryWrapper<>();
        queryWrapper
                .eq("company_id", company.getCompanyId())
                .eq("supplier_status", SUPPLIER_STATUS_ON_SERVICE);
        List<Supplier> supplierList = list(queryWrapper);
        if (SELECT_ZERO != supplierList.size()) {
            return supplierList;
        }
        throw new ServiceBusinessException(HTTP_RETURN_SUCCESS, INQUIRE_EMPOWER_FAIL);
    }

    @Override
    public Boolean deleteSupplier(Supplier supplier) throws Exception {
        LocalDateTime now = LocalDateTime.now();
        Supplier selectSupplier = getById(supplier.getSupplierId());
        if (null != selectSupplier) {
            UpdateWrapper<Supplier> updateWrapper = new UpdateWrapper<>();
            updateWrapper
                    .eq("supplier_id", supplier.getSupplierId())
                    .eq("update_time", supplier.getUpdateTime())
                    .set("supplier_status", supplier.getSupplierStatus())
                    .set("update_time", now);
            if (update(updateWrapper)) {
                return true;
            }
        }
        throw new ServiceBusinessException(HTTP_RETURN_SUCCESS, INQUIRE_EMPOWER_FAIL);
    }

}
