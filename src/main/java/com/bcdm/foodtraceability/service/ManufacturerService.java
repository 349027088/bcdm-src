package com.bcdm.foodtraceability.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.entity.Manufacturer;
import com.baomidou.mybatisplus.extension.service.IService;
import com.bcdm.foodtraceability.entity.SelectPageEntity;

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
     * 获取指定内容条数的生产厂商信息
     *
     * @param selectInfo 查询条件内容
     * @return 查询结果
     * @throws Exception 查找生产厂商信息失败或未查到
     */
    IPage<Manufacturer> getManufacturerList(SelectPageEntity<Manufacturer> selectInfo) throws Exception;

    /**
     * 获取指定的生产厂商信息
     *
     * @param getOneInfo 获取公司的指定生产厂商的Id和公司Id
     * @return 获取的指定生产厂商信息
     * @throws Exception 获取信息失败
     */
    Manufacturer getManufacturerById(Manufacturer getOneInfo) throws Exception;

    /**
     * 新建一个生产厂商
     *
     * @param manufacturer 需要创建的生产厂商信息
     * @return true 添加成功
     * @throws Exception 添加生产厂商信息失败
     */
    Boolean createManufacturer(Manufacturer manufacturer) throws Exception;

    /**
     * 删除一个生产厂商
     *
     * @param manufacturer 需要删除的生产厂商信息
     * @return true 删除成功
     * @throws Exception 删除生产厂商信息失败
     */
    Boolean deleteManufacturer(Manufacturer manufacturer) throws Exception;

    /**
     * 修改生产厂商信息
     *
     * @param manufacturer 需要修改的生产厂商信息
     * @return true 修改成功
     * @throws Exception 修改生产厂商信息失败
     */
    Boolean modifyManufacturer(Manufacturer manufacturer) throws Exception;
}
