package com.bcdm.foodtraceability.service.impl;

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

import static com.bcdm.foodtraceability.common.Constants.*;
import static com.bcdm.foodtraceability.common.MessageConstants.*;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;

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
        selectInfo.getSelectInfo().setGoodsModel();
        if (null != selectInfo.getSelectInfo()) {
            selectInfo.setPageInfo(goodsMapper.selectGoodsPage((Page<GoodsModel>) selectInfo.getPageInfo(), selectInfo.getSelectInfo()));
            if (SELECT_ZERO != selectInfo.getPageInfo().getTotal()) {
                return selectInfo.getPageInfo();
            }
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, SELECT_GOODS_INFO_FAIL);
    }

    @Override
    public Boolean createGoods(GoodsModel goods) throws Exception {
        goods.setGoodsModel();
        if (Boolean.TRUE.equals(checkSubInfo(goods)) && Boolean.FALSE.equals(checkGoods(goods, SELECT_CHECK_PARAM_CREATE))) {
            LocalDateTime now = LocalDateTime.now();
            goods.setGoodsLevel(GOODS_LEVEL_ZERO);
            goods.setGoodsStatus(GOODS_STATUS_ON_SERVICE);
            goods.setCreateTime(now);
            goods.setUpdateTime(now);
            if (GET_ONE.equals(goodsMapper.saveGoodsModel(goods))) {
                barcodeService.createBarcode(goods);
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, ADD_GOODS_INFO_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, FIND_GOODS_NAME_BY_COMPANY_FAIL2);
    }

    @Override
    public Boolean modifyGoods(GoodsModel goods) throws Exception {
        goods.setGoodsModel();
        if (Boolean.TRUE.equals(checkSubInfo(goods)) && Boolean.TRUE.equals(checkGoods(goods, SELECT_CHECK_PARAM_MODIFY))) {
            if (GET_ONE.equals(goodsMapper.modifyGoodsModel(goods))) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_GOODS_INFO_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, FIND_GOODS_NAME_BY_COMPANY_FAIL1);
    }

    @Override
    public Boolean deleteGoods(GoodsModel goods) throws Exception {
        goods.setGoodsModel();
        if (GET_ONE.equals(goodsMapper.deleteGoodsModel(goods))) {
            return true;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, DELETE_GOODS_INFO_FAIL);
    }

    /**
     * 查询商品关联得信息是否存在
     *
     * @param goods 需要查询创建得商品信息
     * @return 如果存在返回true
     */
    private Boolean checkSubInfo(GoodsModel goods) throws Exception {
        if (GET_ONE.equals(goodsMapper.checkToInsert(goods))) {
            return true;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, CHECK_INFO_FAIL);
    }

    /**
     * 查询传入公司ID和生产厂商的名称是否在该公司存在
     *
     * @param goods    希望操作的增删改查生产厂商信息
     * @param selectId 操作ID create 1 modify 2 delete 3
     * @return 查询结果为0返回false，查询结果大于0返回true
     */
    private Boolean checkGoods(GoodsModel goods, Integer selectId) {
        switch (selectId) {
            case 1:
                return SELECT_ZERO < goodsMapper.countGoodsName(goods);
            case 2:
                GoodsModel goodsNameByGoodsId = goodsMapper.getGoodsNameByGoodsId(goods);
                if (null == goodsNameByGoodsId) {
                    return false;

                } else if (goodsNameByGoodsId.getGoodsName().equals(goods.getGoodsName())) {
                    return true;
                }
                return SELECT_ZERO.equals(goodsMapper.countGoodsName(goods));
        }
        return null;
    }
}
