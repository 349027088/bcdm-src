package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.bcdm.foodtraceability.entity.Manufacturer;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.ManufacturerMapper;
import com.bcdm.foodtraceability.service.ManufacturerService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;

import static com.bcdm.foodtraceability.common.Constants.*;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 生产厂商服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Service
public class ManufacturerServiceImpl extends ServiceImpl<ManufacturerMapper, Manufacturer> implements ManufacturerService {


    @Override
    public List<Manufacturer> getManufacturerList(Integer companyId) throws Exception {
        List<Manufacturer> manufacturerList = list(new QueryWrapper<Manufacturer>().eq("company_id", companyId));
        if (SELECT_ZERO == manufacturerList.size()) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, SELECT_MANUFACTURER_INFO_FAIL);
        }
        return manufacturerList;
    }

    @Override
    public Boolean createManufacturer(Manufacturer manufacturer) throws Exception {
        if (Boolean.FALSE.equals(checkManufacturer(manufacturer, SELECT_CHECK_PARAM_CREATE))) {
            LocalDateTime now = LocalDateTime.now();
            manufacturer.setCreateTime(now);
            manufacturer.setUpdateTime(now);
            if (save(manufacturer)) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, ADD_MANUFACTURER_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, FIND_MANUFACTURER_NAME_BY_COMPANY_FAIL2);
    }

    @Override
    public Boolean deleteManufacturer(Manufacturer manufacturer) throws Exception {
        if (Boolean.TRUE.equals(checkManufacturer(manufacturer, SELECT_CHECK_PARAM_DELETE))) {
            if (removeById(manufacturer.getManufacturerId())) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, DELETE_MANUFACTURER_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, FIND_MANUFACTURER_NAME_BY_COMPANY_FAIL1);
    }

    @Override
    public Boolean modifyManufacturer(Manufacturer manufacturer) throws Exception {
        if (Boolean.TRUE.equals(checkManufacturer(manufacturer, SELECT_CHECK_PARAM_MODIFY))) {
            UpdateWrapper<Manufacturer> goodsTypeQueryWrapper = new UpdateWrapper<>();
            goodsTypeQueryWrapper
                    .eq("company_id", manufacturer.getCompanyId())
                    .eq("manufacturer_id", manufacturer.getManufacturerId())
                    .eq("update_time", manufacturer.getUpdateTime())
                    .set("update_time", LocalDateTime.now())
                    .set("manufacturer_name", manufacturer.getManufacturerName());
            if (update(goodsTypeQueryWrapper)) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_MANUFACTURER_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, FIND_MANUFACTURER_NAME_BY_COMPANY_FAIL1);
    }

    /**
     * 查询传入公司ID和生产厂商的名称是否在该公司存在
     *
     * @param manufacturer 希望操作的增删改查生产厂商信息
     * @param selectId  操作ID create 1 modify 2 delete 3
     * @return 查询结果为0返回false，查询结果大于0返回true
     */
    private Boolean checkManufacturer(Manufacturer manufacturer, Integer selectId) {
        switch (selectId) {
            case 1:
            case 2:
                return SELECT_ZERO < count(new QueryWrapper<Manufacturer>().eq("company_id", manufacturer.getCompanyId()).eq("manufacturer_name", manufacturer.getManufacturerName()));
            case 3:
                return GET_ONE == count(new QueryWrapper<Manufacturer>().eq("company_id", manufacturer.getCompanyId()).eq("manufacturer_name", manufacturer.getManufacturerName()));
        }
        return null;
    }
}
