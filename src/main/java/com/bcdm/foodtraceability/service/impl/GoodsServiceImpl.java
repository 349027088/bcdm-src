package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.bcdm.foodtraceability.entity.*;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.GoodsMapper;
import com.bcdm.foodtraceability.service.BarcodeService;
import com.bcdm.foodtraceability.service.GoodsService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;

import static com.bcdm.foodtraceability.common.Constants.*;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 商品服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Service
public class GoodsServiceImpl extends ServiceImpl<GoodsMapper, Goods> implements GoodsService {

    private final GoodsMapper goodsMapper;

    private final BarcodeService barcodeService;

    public GoodsServiceImpl(GoodsMapper goodsMapper, BarcodeService barcodeService) {
        this.goodsMapper = goodsMapper;
        this.barcodeService = barcodeService;
    }

    @Override
    public IPage<GoodsModel> getGoodsListByCompany(SelectPageEntity<GoodsModel> selectInfo) throws Exception {
        if (null != selectInfo.getSelectInfo()) {
            selectInfo.setPageInfo(goodsMapper.selectGoodsPage((Page<GoodsModel>) selectInfo.getPageInfo(), selectInfo.getSelectInfo()));
            if (SELECT_ZERO != selectInfo.getPageInfo().getTotal()) {
                return selectInfo.getPageInfo();
            }
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, SELECT_GOODS_INFO_FAIL);
    }

    @Override
    public Boolean createGoods(Goods goods) throws Exception {
        if (Boolean.TRUE.equals(checkSubInfo(goods)) && Boolean.FALSE.equals(checkGoods(goods, SELECT_CHECK_PARAM_CREATE))) {
            LocalDateTime now = LocalDateTime.now();
            goods.setGoodsLevel(GOODS_LEVEL_ZERO);
            goods.setGoodsStatus(GOODS_STATUS_ON_SERVICE);
            goods.setCreateTime(now);
            goods.setUpdateTime(now);
            if (save(goods)) {
                barcodeService.createBarcode(goods.getGoodsId());
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, ADD_GOODS_INFO_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, FIND_GOODS_NAME_BY_COMPANY_FAIL2);
    }

    @Override
    public Boolean modifyGoods(Goods goods) throws Exception {
        if (Boolean.TRUE.equals(checkSubInfo(goods)) && Boolean.TRUE.equals(checkGoods(goods, SELECT_CHECK_PARAM_MODIFY))) {
            UpdateWrapper<Goods> goodsTypeQueryWrapper = new UpdateWrapper<>();
            goodsTypeQueryWrapper
                    .eq("company_id", goods.getCompanyId())
                    .eq("goods_id", goods.getGoodsId())
                    .eq("update_time", goods.getUpdateTime())
                    .set("update_time", LocalDateTime.now())
                    .set("goods_type_id", goods.getGoodsTypeId())
                    .set("supplier_id", goods.getSupplierId())
                    .set("manufacturer_id", goods.getManufacturerId())
                    .set("goods_name", goods.getGoodsName())
                    .set("goods_explain", goods.getGoodsExplain())
                    .set("raw_material", goods.getRawMaterial())
                    .set("place_of_production", goods.getPlaceOfProduction())
                    .set("quality_guarantee", goods.getQualityGuarantee())
                    .set("manufacture_date", goods.getManufactureDate())
                    .set("product_icon", goods.getProductIcon());
            if (update(goodsTypeQueryWrapper)) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_GOODS_INFO_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, FIND_GOODS_NAME_BY_COMPANY_FAIL1);
    }

    @Override
    public boolean deleteGoods(Goods goods) throws Exception {
        if (Boolean.TRUE.equals(checkGoods(goods, SELECT_CHECK_PARAM_DELETE))) {
            if (removeById(goods.getGoodsId())) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, DELETE_GOODS_INFO_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, FIND_GOODS_NAME_BY_COMPANY_FAIL3);
    }

    @Override
    public int deleteGoodsList(Company company, List<Goods> goodsList) throws Exception {
        return 0;
    }

    @Override
    public List<Goods> createGoodsList(Company company, List<Goods> goodsList) throws Exception {
        return null;
    }

    @Override
    public List<Goods> modifyGoodsList(Company company, List<Goods> goodsList) throws Exception {
        return null;
    }

    private Boolean checkSubInfo(Goods goods) {
        return GET_ONE.equals(goodsMapper.checkToInsert(goods));
    }

    /**
     * 查询传入公司ID和生产厂商的名称是否在该公司存在
     *
     * @param goods    希望操作的增删改查生产厂商信息
     * @param selectId 操作ID create 1 modify 2 delete 3
     * @return 查询结果为0返回false，查询结果大于0返回true
     */
    private Boolean checkGoods(Goods goods, Integer selectId) {
        switch (selectId) {
            case 1:
                return SELECT_ZERO < count(new QueryWrapper<Goods>()
                        .eq("company_id", goods.getCompanyId())
                        .eq("supplier_id", goods.getSupplierId())
                        .eq("goods_name", goods.getGoodsName()));
            case 2:
                Goods selectGoods = getOne(new QueryWrapper<Goods>()
                        .eq("company_id", goods.getCompanyId())
                        .eq("goods_id", goods.getGoodsId()));
                if (null != selectGoods) {
                    if (selectGoods.getGoodsName().equals(goods.getGoodsName())) {
                        return true;
                    }
                } else {
                    return false;

                }
                return SELECT_ZERO == count(new QueryWrapper<Goods>()
                        .eq("company_id", goods.getCompanyId())
                        .eq("supplier_id", goods.getSupplierId())
                        .eq("goods_name", goods.getGoodsName()));
            case 3:
                return GET_ONE == count(new QueryWrapper<Goods>()
                        .eq("company_id", goods.getCompanyId())
                        .eq("goods_id", goods.getGoodsId()));
        }
        return null;
    }
}
