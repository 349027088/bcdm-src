package com.bcdm.foodtraceability.service.impl;

import com.bcdm.foodtraceability.entity.GoodsType;
import com.bcdm.foodtraceability.mapper.GoodsTypeMapper;
import com.bcdm.foodtraceability.service.GoodsTypeService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Service
public class GoodsTypeServiceImpl extends ServiceImpl<GoodsTypeMapper, GoodsType> implements GoodsTypeService {

    @Override
    public boolean isGoodsType(int goodsTypeId) throws Exception {
        return false;
    }

    @Override
    public List<GoodsType> getGoodsTypeList(int status) throws Exception {
        return null;
    }


    @Override
    public GoodsType createGoodsType(GoodsType goodsType) throws Exception {
        return null;
    }

    @Override
    public GoodsType modifyGoodsType(GoodsType goodsType) throws Exception {
        return null;
    }
}
