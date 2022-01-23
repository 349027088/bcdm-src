package com.bcdm.foodtraceability.service.impl;

import com.bcdm.foodtraceability.entity.Company;
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
    public List<GoodsType> getGoodsTypeList(Company company) throws Exception {
        return null;
    }


    @Override
    public Boolean createGoodsType(GoodsType goodsType) throws Exception {
        return null;
    }

    @Override
    public Boolean deleteGoodsType(GoodsType goodsType) throws Exception {
        return null;
    }

}
