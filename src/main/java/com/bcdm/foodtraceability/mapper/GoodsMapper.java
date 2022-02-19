package com.bcdm.foodtraceability.mapper;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.bcdm.foodtraceability.entity.Goods;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.bcdm.foodtraceability.entity.GoodsModel;
import org.apache.ibatis.annotations.Mapper;

/**
 * <p>
 * 商品信息Mapper 接口
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Mapper
public interface GoodsMapper extends BaseMapper<Goods> {

    /**
     * 参数条件查询
     *
     * @param page       分页条件
     * @param goodsModel 查询条件
     * @return 分页后的商品信息
     */
    IPage<GoodsModel> selectGoodsPage(Page<GoodsModel> page, GoodsModel goodsModel);

    /**
     * 验证插入商品信息的正确性
     *
     * @param goods 需要验证的商品信息
     * @return 返回结果是否为1
     */
    Integer checkToInsert(GoodsModel goods);

    /**
     * 插入新的商品信息
     *
     * @param goodsModel 需要插入的商品信息
     * @return 返回1为插入成功
     */
    Integer saveGoodsModel(GoodsModel goodsModel);

    /**
     * 修改商品信息
     *
     * @param goodsModel 需要插入的商品信息
     * @return 返回1为插入成功
     */
    Integer modifyGoodsModel(GoodsModel goodsModel);

    /**
     * 删除商品信息
     *
     * @param goodsModel 需要插入的商品信息
     * @return 返回1为插入成功
     */
    Integer deleteGoodsModel(GoodsModel goodsModel);

    /**
     * 查询商品信息count
     *
     * @param goods 需要删除的商品信息
     * @return 删除是否成功
     */
    Integer countGoodsName(GoodsModel goods);

    /**
     * 通过商品Id查询商品名称
     *
     * @param goods 需要查询的商品Id和table信息
     * @return 查询到的name信息
     */
    GoodsModel getGoodsNameByGoodsId(GoodsModel goods);
}
