package com.bcdm.foodtraceability.mapper;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.bcdm.foodtraceability.entity.CompanyInfoCheck;
import com.bcdm.foodtraceability.entity.Goods;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.bcdm.foodtraceability.entity.GoodsModel;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import org.apache.commons.lang3.StringUtils;
import org.apache.ibatis.annotations.Mapper;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;

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
    Integer checkToInsert(Goods goods);
}
