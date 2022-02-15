package com.bcdm.foodtraceability.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.Goods;
import com.baomidou.mybatisplus.extension.service.IService;
import com.bcdm.foodtraceability.entity.GoodsModel;
import com.bcdm.foodtraceability.entity.SelectPageEntity;

import java.util.List;

/**
 * <p>
 * 商品服务类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
public interface GoodsService extends IService<Goods> {

    /**
     * 获取公司的商品列表
     *
     * @param selectInfo 需要获取商品列表的公司ID
     * @return 获取的商品列表
     * @throws Exception 返回商品列表失败
     */
    IPage<GoodsModel> getGoodsListByCompany(SelectPageEntity<GoodsModel> selectInfo) throws Exception;

    /**
     * 公司添加一个新的商品
     *
     * @param goods   新建商品的信息
     * @return 创建成功的商品
     * @throws Exception 创建商品失败
     */
    Boolean createGoods(Goods goods) throws Exception;

    /**
     * 批量创建商品
     *
     * @param company   创建商品的公司
     * @param goodsList 批量创建的商品信息
     * @return 创建成功的商品列表
     * @throws Exception 创建商品失败
     */
    List<Goods> createGoodsList(Company company, List<Goods> goodsList) throws Exception;

    /**
     * 批量修改商品信息
     *
     * @param company   修改商品信息的公司
     * @param goodsList 批量修改的商品信息
     * @return 修改成功的商品列表
     * @throws Exception 修改失败的商品列表
     */
    List<Goods> modifyGoodsList(Company company, List<Goods> goodsList) throws Exception;

    /**
     * 需要修改的商品信息
     *
     * @param goods 需要修改的商品信息
     * @return 修改成功的商品信息
     * @throws Exception 修改商品信息失败
     */
    Boolean modifyGoods(Goods goods) throws Exception;

    /**
     * 删除一个商品信息
     * @param goods 需要删除的商品信息
     * @return 删除是否成功
     * @throws Exception 删除信息失败
     */
    boolean deleteGoods(Goods goods) throws Exception;

    /**
     * 批量删除商品信息
     * @param company 需要删除商品信息的公司
     * @param goodsList 需要删除的商品列表
     * @return 删除成功条数
     * @throws Exception 删除信息失败
     */
    int deleteGoodsList(Company company,List<Goods> goodsList)throws Exception;


    /**
     * 查询指定的商品信息
     *
     * @param goodsModel 需要查询的商品ID
     * @return 查询到的商品信息
     * @throws Exception 查询商品信息失败
     */
    GoodsModel getGoodsById(GoodsModel goodsModel)throws Exception;
}
