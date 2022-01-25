package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.GoodsType;
import com.bcdm.foodtraceability.mapper.GoodsTypeMapper;
import com.bcdm.foodtraceability.service.GoodsTypeService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;

import static com.bcdm.foodtraceability.common.Constants.SELECT_ZERO;

/**
 * <p>
 * 商品类别服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Service
@Slf4j
public class GoodsTypeServiceImpl extends ServiceImpl<GoodsTypeMapper, GoodsType> implements GoodsTypeService {

    @Override
    public List<GoodsType> getGoodsTypeList(Company company) {
        return list(new QueryWrapper<GoodsType>().eq("company_id", company.getCompanyId()));
    }


    @Override
    public Boolean createGoodsType(GoodsType goodsType) {
        LocalDateTime now = LocalDateTime.now();
        goodsType.setUpdateTime(now);
        goodsType.setCreateTime(now);
        return SELECT_ZERO != count(new QueryWrapper<GoodsType>().eq("goods_type_name", goodsType.getGoodsTypeName())) && save(goodsType);
    }

    @Override
    public Boolean deleteGoodsType(GoodsType goodsType) {
        return removeById(goodsType.getGoodsTypeId());
    }
}
