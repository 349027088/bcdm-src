package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.entity.Manufacturer;
import com.bcdm.foodtraceability.entity.SelectPageEntity;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.ManufacturerMapper;
import com.bcdm.foodtraceability.service.ManufacturerService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

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
    public IPage<Manufacturer> getManufacturerList(SelectPageEntity<Manufacturer> selectInfo) throws Exception {
        QueryWrapper<Manufacturer> manufacturerQueryWrapper = new QueryWrapper<>();
        if (!StringUtils.isEmpty(selectInfo.getSelectName())) {
            manufacturerQueryWrapper.likeRight("manufacturer_name", selectInfo.getSelectName());
        }
        manufacturerQueryWrapper.eq("company_id", selectInfo.getCompanyId());
        selectInfo.setPageInfo(page(selectInfo.getPageInfo(), manufacturerQueryWrapper));
        if (SELECT_ZERO == selectInfo.getPageInfo().getTotal()) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, SELECT_MANUFACTURER_INFO_FAIL);
        }
        return selectInfo.getPageInfo();
    }

    @Override
    public Manufacturer getManufacturerById(Manufacturer getOneInfo) throws Exception {
        Manufacturer manufacturer = getOne(new QueryWrapper<Manufacturer>()
                .eq("company_id", getOneInfo.getCompanyId())
                .eq("manufacturer_id", getOneInfo.getManufacturerId()));
        if (null != manufacturer) {
            return manufacturer;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, SELECT_MANUFACTURER_INFO_FAIL);
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
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, FIND_MANUFACTURER_NAME_BY_COMPANY_FAIL3);
    }

    /**
     * 查询传入公司ID和生产厂商的名称是否在该公司存在
     *
     * @param manufacturer 希望操作的增删改查生产厂商信息
     * @param selectId     操作ID create 1 modify 2 delete 3
     * @return 查询结果为0返回false，查询结果大于0返回true
     */
    private Boolean checkManufacturer(Manufacturer manufacturer, Integer selectId) {
        switch (selectId) {
            case 1:
                return SELECT_ZERO < count(new QueryWrapper<Manufacturer>().eq("company_id", manufacturer.getCompanyId()).eq("manufacturer_name", manufacturer.getManufacturerName()));
            case 2:
                return GET_ONE == count(new QueryWrapper<Manufacturer>().eq("company_id", manufacturer.getCompanyId()).eq("manufacturer_id", manufacturer.getManufacturerName())) &&
                        SELECT_ZERO == count(new QueryWrapper<Manufacturer>().eq("company_id", manufacturer.getCompanyId()).eq("manufacturer_name", manufacturer.getManufacturerName()));
            case 3:
                return GET_ONE == count(new QueryWrapper<Manufacturer>().eq("company_id", manufacturer.getCompanyId()).eq("manufacturer_id", manufacturer.getManufacturerName()));
        }
        return null;
    }
}
