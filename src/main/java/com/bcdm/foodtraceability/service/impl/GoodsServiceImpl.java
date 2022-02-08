package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.bcdm.foodtraceability.common.CreateUUID;
import com.bcdm.foodtraceability.entity.*;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.GoodsMapper;
import com.bcdm.foodtraceability.service.GoodsService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
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
@Slf4j
public class GoodsServiceImpl extends ServiceImpl<GoodsMapper, Goods> implements GoodsService {

    private final GoodsMapper goodsMapper;

    public GoodsServiceImpl(GoodsMapper goodsMapper) {
        this.goodsMapper = goodsMapper;
    }

    @Override
    public IPage<GoodsModel> getGoodsListByCompany(SelectPageEntity<GoodsModel> selectInfo) throws Exception {
        selectInfo.setPageInfo(goodsMapper.selectGoodsPage((Page<GoodsModel>) selectInfo.getPageInfo(), selectInfo.getSelectInfo()));
        if (SELECT_ZERO == selectInfo.getPageInfo().getTotal()) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, SELECT_GOODS_INFO_FAIL);
        }
        return selectInfo.getPageInfo();
    }

    @Override
    public Boolean createGoods(Goods goods) throws Exception {
        if (Boolean.TRUE.equals(checkSubInfo(goods)) && Boolean.FALSE.equals(checkGoods(goods, SELECT_CHECK_PARAM_CREATE))) {
            LocalDateTime now = LocalDateTime.now();
            goods.setGoodsLevel(GOODS_LEVEL_ZERO);
            goods.setGoodsStatus(GOODS_STATUS_ON_SERVICE);
            goods.setCreateTime(now);
            goods.setUpdateTime(now);
            goods.setBarcodeNumber(CreateUUID.getUUID());
            if (save(goods)) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, ADD_MANUFACTURER_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, FIND_MANUFACTURER_NAME_BY_COMPANY_FAIL2);
    }

    @Override
    public Boolean modifyGoods(Goods goods) throws Exception {
        return null;
    }

    @Override
    public boolean deleteGoods(Goods goods) throws Exception {
        if (Boolean.TRUE.equals(checkGoods(goods, SELECT_CHECK_PARAM_DELETE))) {
            if (removeById(goods.getGoodsId())) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, DELETE_MANUFACTURER_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, FIND_MANUFACTURER_NAME_BY_COMPANY_FAIL1);
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
                return GET_ONE == count(new QueryWrapper<Goods>()
                        .eq("company_id", goods.getCompanyId())
                        .eq("goods_id", goods.getGoodsId())) &&
                        SELECT_ZERO == count(new QueryWrapper<Goods>()
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
