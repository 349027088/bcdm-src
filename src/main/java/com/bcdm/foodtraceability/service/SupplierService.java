package com.bcdm.foodtraceability.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.entity.SelectPageEntity;
import com.bcdm.foodtraceability.entity.Supplier;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * <p>
 * 供应商服务类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
public interface SupplierService extends IService<Supplier> {

    /**
     * 获取某条件的公司未删除供应商信息
     *
     * @param selectInfo 查询条件内容
     * @return 查询结果
     * @throws Exception 查找供应商信息失败或未查到
     */
    IPage<Supplier> getSupplierList(SelectPageEntity<Supplier> selectInfo) throws Exception;

    /**
     * 获取指定的供应商信息
     *
     * @param supplier 获取公司的指定供应商的Id和公司Id
     * @return 获取的指定供应商信息
     * @throws Exception 获取信息失败
     */
    Supplier getSupplierById(Supplier supplier) throws Exception;

    /**
     * 创建一个供应商
     *
     * @param supplier 需要创建的供应商信息
     * @return true 添加成功
     * @throws Exception 添加供应商信息失败
     */
    Boolean createSupplier(Supplier supplier) throws Exception;

    /**
     * 修改供应商信息
     *
     * @param supplier 需要修改的供应商信息
     * @return true 删除成功
     * @throws Exception 删除供应商信息失败
     */
    Boolean modifySupplier(Supplier supplier) throws Exception;

    /**
     * 删除供应商信息
     *
     * @param supplier 需要删除的供应商信息
     * @return true 修改成功
     * @throws Exception 修改供应商信息失败
     */
    Boolean deleteSupplier(Supplier supplier) throws Exception;
}
