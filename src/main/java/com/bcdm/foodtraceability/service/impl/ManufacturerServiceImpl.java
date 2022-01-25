package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.bcdm.foodtraceability.entity.Manufacturer;
import com.bcdm.foodtraceability.mapper.ManufacturerMapper;
import com.bcdm.foodtraceability.service.ManufacturerService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;

import static com.bcdm.foodtraceability.common.Constants.SELECT_ZERO;

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
    public List<Manufacturer> getManufacturerList(Integer companyId) {
        return list(new QueryWrapper<Manufacturer>().eq("company_id", companyId));
    }

    @Override
    public Boolean createManufacturer(Manufacturer manufacturer) {
        LocalDateTime now = LocalDateTime.now();
        manufacturer.setCreateTime(now);
        manufacturer.setUpdateTime(now);
        return SELECT_ZERO != count(new QueryWrapper<Manufacturer>().eq("company_id", manufacturer.getCompanyId()).eq("manufacturer_name",manufacturer.getManufacturerName())) && save(manufacturer);
    }

    @Override
    public Boolean deleteManufacturer(Manufacturer manufacturer) {
        return SELECT_ZERO != count(new QueryWrapper<Manufacturer>().eq("company_id", manufacturer.getCompanyId())) && removeById(manufacturer.getManufacturerId());
    }
}
