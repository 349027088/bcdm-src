package com.bcdm.foodtraceability.service;

import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.Manufacturer;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * <p>
 * 生产产地服务类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
public interface ManufacturerService extends IService<Manufacturer> {

    /**
     * 生产厂商列表查询
     *
     * @param companyId 需要获得生产厂商的列表
     * @return 所有生产厂商
     */
    List<Manufacturer> getManufacturerList(Integer companyId) throws Exception;

    /**
     * 新建一个生产厂商
     *
     * @param manufacturer 需要创建的生产厂商信息
     * @return 创建成功的生产厂商的信息
     */
    Boolean createManufacturer(Manufacturer manufacturer) throws Exception;

    /**
     * 删除一个生产厂商
     *
     * @param manufacturer 需要删除的生产厂商信息
     * @return 删除结果
     */
    Boolean deleteManufacturer(Manufacturer manufacturer) throws Exception;

    Boolean modifyManufacturer(Manufacturer manufacturer) throws Exception;
}
