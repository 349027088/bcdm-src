package com.bcdm.foodtraceability.service;

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
     * @return 所有生产厂商
     * @throws Exception 查询生产厂商信息失败
     */
    List<Manufacturer> getManufacturer() throws Exception;

    /**
     * 新建一个生产厂商
     *
     * @param manufacturer 需要创建的生产厂商信息
     * @return 创建成功的生产厂商的信息
     * @throws Exception 创建新的生产厂商信息失败
     */
    Manufacturer createManufacturer(Manufacturer manufacturer) throws Exception;

    /**
     * 修改生产厂商的信息
     *
     * @param manufacturer 需要被修改成的信息
     * @return 修改成功的生产厂商信息
     * @throws Exception 修改生产厂商信息失败
     */
    Manufacturer modifyManufacturer(Manufacturer manufacturer) throws Exception;

}
