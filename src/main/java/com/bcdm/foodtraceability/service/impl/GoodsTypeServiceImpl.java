package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.GoodsType;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.GoodsTypeMapper;
import com.bcdm.foodtraceability.service.GoodsTypeService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.util.List;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

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
    public List<GoodsType> getGoodsTypeList(Company company){
        QueryWrapper<GoodsType> goodsTypeQueryWrapper = new QueryWrapper<>();
        goodsTypeQueryWrapper.eq("company_id",company.getCompanyId());
        return list(goodsTypeQueryWrapper);
    }


    @Override
    public Boolean createGoodsType(GoodsType goodsType) throws Exception {
        if (!save(goodsType)){
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, ADD_GOODSTYPE_FAILED);
        }
        return true;
    }

    @Override
    public Boolean deleteGoodsType(GoodsType goodsType) throws Exception {
        if (!removeById(goodsType.getGoodsTypeId())){
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, DELETE_GOODSTYPE_FAILED);
        }
        return true;
    }

}
