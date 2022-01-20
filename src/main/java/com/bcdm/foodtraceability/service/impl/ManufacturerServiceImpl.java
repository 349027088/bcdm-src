package com.bcdm.foodtraceability.service.impl;

import com.bcdm.foodtraceability.entity.Manufacturer;
import com.bcdm.foodtraceability.mapper.ManufacturerMapper;
import com.bcdm.foodtraceability.service.ManufacturerService;
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
public class ManufacturerServiceImpl extends ServiceImpl<ManufacturerMapper, Manufacturer> implements ManufacturerService {


    @Override
    public List<Manufacturer> getManufacturer() throws Exception {
        return null;
    }

    @Override
    public Manufacturer createManufacturer(Manufacturer manufacturer) throws Exception {
        return null;
    }

    @Override
    public Manufacturer modifyManufacturer(Manufacturer manufacturer) throws Exception {
        return null;
    }
}
