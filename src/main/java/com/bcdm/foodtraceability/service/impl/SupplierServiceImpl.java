package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.entity.SelectPageEntity;
import com.bcdm.foodtraceability.entity.Supplier;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.SupplierMapper;
import com.bcdm.foodtraceability.service.SupplierService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.commons.lang3.StringUtils;
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
    public IPage<Supplier> getSupplierList(SelectPageEntity<Supplier> selectInfo) throws Exception {
        QueryWrapper<Supplier> queryWrapper = new QueryWrapper<>();
        if (!StringUtils.isEmpty(selectInfo.getSelectName())) {
            queryWrapper.likeRight("supplier_name", selectInfo.getSelectName());
        }
        queryWrapper
                .eq("company_id", selectInfo.getCompanyId())
                .eq("supplier_status", SUPPLIER_STATUS_ON_SERVICE)
                .eq("supplier_level", SUPPLIER_LEVEL_ON_SERVICE);
        IPage<Supplier> supplierList = page(selectInfo.getPageInfo(), queryWrapper);
        if (SELECT_ZERO == supplierList.getTotal()) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, SELECT_SUPPLIER_INFO_FAIL);
        }
        return supplierList;
    }

    @Override
    public Supplier getSupplierById(Supplier supplier) throws Exception {
        Supplier selectSupplier = getOne(new QueryWrapper<Supplier>()
                .eq("company_id", supplier.getCompanyId())
                .eq("supplier_id", supplier.getSupplierId())
                .eq("supplier_status", SUPPLIER_STATUS_ON_SERVICE)
                .eq("supplier_level", SUPPLIER_LEVEL_ON_SERVICE));
        if (null != selectSupplier) {
            return selectSupplier;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, SELECT_SUPPLIER_INFO_FAIL);
    }

    @Override
    public Boolean createSupplier(Supplier supplier) throws Exception {
        if (Boolean.FALSE.equals(checkSupplier(supplier, SELECT_CHECK_PARAM_CREATE))) {
            LocalDateTime now = LocalDateTime.now();
            supplier.setSupplierStatus(SUPPLIER_STATUS_ON_SERVICE);
            supplier.setSupplierLevel(SUPPLIER_LEVEL_ON_SERVICE);
            supplier.setCreateTime(now);
            supplier.setUpdateTime(now);
            if (save(supplier)) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_SUPPLIER_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, FIND_SUPPLIER_NAME_BY_COMPANY_FAIL2);
    }

    @Override
    public Boolean modifySupplier(Supplier supplier) throws Exception {
        if (Boolean.TRUE.equals(checkSupplier(supplier, SELECT_CHECK_PARAM_DELETE))) {
            UpdateWrapper<Supplier> updateWrapper = new UpdateWrapper<>();
            updateWrapper
                    .eq("supplier_id", supplier.getSupplierId())
                    .eq("update_time", supplier.getUpdateTime())
                    .set("supplier_phone_number", supplier.getSupplierPhoneNumber())
                    .set("main_business", supplier.getMainBusiness())
                    .set("business_license", supplier.getBusinessLicense())
                    .set("health_permit", supplier.getHealthPermit())
                    .set("update_time", LocalDateTime.now());
            if (update(updateWrapper)) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_SUPPLIER_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, FIND_SUPPLIER_NAME_BY_COMPANY_FAIL1);
    }

    @Override
    public Boolean deleteSupplier(Supplier supplier) throws Exception {
        if (Boolean.TRUE.equals(checkSupplier(supplier, SELECT_CHECK_PARAM_DELETE))) {
            UpdateWrapper<Supplier> updateWrapper = new UpdateWrapper<>();
            updateWrapper
                    .eq("company_id", supplier.getCompanyId())
                    .eq("supplier_id", supplier.getSupplierId())
                    .eq("update_time", supplier.getUpdateTime())
                    .set("supplier_status", SUPPLIER_LEVEL_OUT_OF_SERVICE)
                    .set("update_time", LocalDateTime.now());
            if (update(updateWrapper)) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_SUCCESS, DELETE_SUPPLIER_INFO_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_SUCCESS, FIND_SUPPLIER_NAME_BY_COMPANY_FAIL3);
    }

    /**
     * 查询传入公司ID和供应商的名称是否在该公司存在
     *
     * @param supplier 希望操作的增删改查供应商信息
     * @param selectId 操作ID create 1 modify 2 delete 3
     * @return 查询结果为0返回false，查询结果大于0返回true
     */
    private Boolean checkSupplier(Supplier supplier, Integer selectId) {
        switch (selectId) {
            case 1:
                return SELECT_ZERO < count(new QueryWrapper<Supplier>()
                        .eq("company_id", supplier.getCompanyId())
                        .eq("supplier_name", supplier.getSupplierName())
                        .eq("supplier_level", SUPPLIER_LEVEL_ON_SERVICE));
            case 2:
                return GET_ONE == count(new QueryWrapper<Supplier>()
                        .eq("company_id", supplier.getCompanyId())
                        .eq("supplier_id", supplier.getSupplierId())
                        .eq("supplier_level", SUPPLIER_LEVEL_ON_SERVICE)) &&
                        SELECT_ZERO == count(new QueryWrapper<Supplier>()
                                .eq("company_id", supplier.getCompanyId())
                                .eq("supplier_name", supplier.getSupplierName())
                                .eq("supplier_level", SUPPLIER_LEVEL_ON_SERVICE));
            case 3:
                return GET_ONE == count(new QueryWrapper<Supplier>()
                        .eq("company_id", supplier.getCompanyId())
                        .eq("supplier_id", supplier.getSupplierId())
                        .eq("supplier_status", SUPPLIER_STATUS_ON_SERVICE)
                        .eq("supplier_level", SUPPLIER_LEVEL_ON_SERVICE));
        }
        return null;
    }
}
