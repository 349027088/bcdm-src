package com.bcdm.foodtraceability.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.entity.Goods;
import com.baomidou.mybatisplus.extension.service.IService;
import com.bcdm.foodtraceability.entity.GoodsModel;
import com.bcdm.foodtraceability.entity.SelectPageEntity;

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
    Boolean createGoods(GoodsModel goods) throws Exception;

    /**
     * 需要修改的商品信息
     *
     * @param goods 需要修改的商品信息
     * @return 修改成功的商品信息
     * @throws Exception 修改商品信息失败
     */
    Boolean modifyGoods(GoodsModel goods) throws Exception;

    /**
     * 删除一个商品信息
     * @param goods 需要删除的商品信息
     * @return 删除是否成功
     * @throws Exception 删除信息失败
     */
    Boolean deleteGoods(GoodsModel goods) throws Exception;

}
