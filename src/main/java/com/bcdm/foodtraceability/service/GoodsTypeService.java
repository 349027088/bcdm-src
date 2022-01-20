package com.bcdm.foodtraceability.service;

import com.bcdm.foodtraceability.entity.GoodsType;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * <p>
 * 服务类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
public interface GoodsTypeService extends IService<GoodsType> {

    /**
     * 判断该商品种类是否存在
     * @param goodsTypeId 商品种类Code
     * @return 商品种类是否存在
     * @throws Exception 判断种类失败
     */
    boolean isGoodsType(int goodsTypeId) throws Exception;

    /**
     * 查询xx状态下的所有种类信息
     * @param status 需要查询商品种类的状态
     * @return 返回当前通过审核的所有种类信息
     * @throws Exception 查询商品种类信息失败
     */
    List<GoodsType> getGoodsTypeList(int status) throws Exception;

    /**
     * 创建一个新的商品种类
     * @param goodsType 商品的种类信息
     * @return 创建成功的商品种类
     * @throws Exception 创建商品种类失败
     */
    GoodsType createGoodsType(GoodsType goodsType) throws Exception;

    /**
     * 修改商品种类信息或者通过商品种类信息的审核
     * @param goodsType 变更结束的商品种类信息
     * @return 变成成功的商品种类信息
     * @throws Exception 变更商品种类信息操作失败
     */
    GoodsType modifyGoodsType(GoodsType goodsType) throws Exception;

}
