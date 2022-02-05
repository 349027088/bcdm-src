package com.bcdm.foodtraceability.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.entity.GoodsType;
import com.baomidou.mybatisplus.extension.service.IService;
import com.bcdm.foodtraceability.entity.SelectPageEntity;

/**
 * <p>
 * 商品种别服务类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
public interface GoodsTypeService extends IService<GoodsType> {

    /**
     * 获取指定内容条数的商品种类信息
     *
     * @param selectInfo 查询条件内容
     * @return 查找结果
     * @throws Exception 查找商品种类信息失败或者未查到
     */
    IPage<GoodsType> getGoodsTypeList(SelectPageEntity<GoodsType> selectInfo) throws Exception;

    /**
     * 获取公司指定的商品种类
     *
     * @param getOneInfo 获取公司的指定商品种类的Id和公司Id
     * @return 查询成功的商品种类
     * @throws Exception 获取信息失败
     */
    GoodsType getGoodsTypeById(GoodsType getOneInfo) throws Exception;

    /**
     * 创建一个新的商品种类
     *
     * @param goodsType 商品的种类信息
     * @return true 添加成功
     * @throws Exception 创建商品种类失败
     */
    Boolean createGoodsType(GoodsType goodsType) throws Exception;

    /**
     * 删除商品种类信息
     *
     * @param goodsType 需要删除的商品种类信息
     * @return true 删除成功
     * @throws Exception 删除商品种类信息操作失败
     */
    Boolean deleteGoodsType(GoodsType goodsType) throws Exception;

    /**
     * 修改商品种类信息
     *
     * @param goodsType 变更的商品种类信息
     * @return true 修改成功
     * @throws Exception 变更商品种类信息操作失败
     */
    Boolean modifyGoodsType(GoodsType goodsType) throws Exception;
}
