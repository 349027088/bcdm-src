package com.bcdm.foodtraceability.service.impl;

import com.bcdm.foodtraceability.entity.Goods;
import com.bcdm.foodtraceability.mapper.GoodsMapper;
import com.bcdm.foodtraceability.service.GoodsService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.bcdm.foodtraceability.service.GoodsTypeService;
import com.bcdm.foodtraceability.service.IconService;
import org.springframework.stereotype.Service;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Service
public class GoodsServiceImpl extends ServiceImpl<GoodsMapper, Goods> implements GoodsService {

    private final IconService iconService;

    private final GoodsTypeService goodsTypeService;


    public GoodsServiceImpl(IconService iconService, GoodsTypeService goodsTypeService) {
        this.iconService = iconService;
        this.goodsTypeService = goodsTypeService;
    }
}
