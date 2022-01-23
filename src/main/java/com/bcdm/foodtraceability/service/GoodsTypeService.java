package com.bcdm.foodtraceability.service;

import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.GoodsType;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

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
     * 获取公司全部的商品种类
     *
     * @param company 公司信息
     * @return 查询成功的商品种类
     */
    List<GoodsType> getGoodsTypeList(Company company);

    /**
     * 创建一个新的商品种类
     *
     * @param goodsType 商品的种类信息
     * @return 创建成功的商品种类
     * @throws Exception 创建商品种类失败
     */
    Boolean createGoodsType(GoodsType goodsType) throws Exception;

    /**
     * 修改商品种类信息
     *
     * @param goodsType 变更的商品种类信息
     * @return 变成成功的商品种类信息
     * @throws Exception 变更商品种类信息操作失败
     */
    Boolean deleteGoodsType(GoodsType goodsType) throws Exception;

}
