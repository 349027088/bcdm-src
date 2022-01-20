package com.bcdm.foodtraceability.service;

import com.bcdm.foodtraceability.entity.Supplier;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * <p>
 *  供应商服务类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
public interface SupplierService extends IService<Supplier> {

    /**
     * 创建一个供应商
     * @param supplier 需要创建的供应商信息
     * @return 创建成功的供应商信息
     * @throws Exception 创建供应商失败
     */
    Supplier createSupplier(Supplier supplier)throws Exception;

    /**
     *  修改供应商信息
     * @param supplier 需要修改的供应商信息
     * @return 修改成功的供应商信息
     * @throws Exception 修改供应商信息失败
     */
    Supplier modifySupplier(Supplier supplier)throws Exception;

    /**
     * 获取供应商信息列表
     * @return 所有供应商的信息
     * @throws Exception 查询供应商信息失败
     */
    List<Supplier> getSupplierList()throws Exception;

}
