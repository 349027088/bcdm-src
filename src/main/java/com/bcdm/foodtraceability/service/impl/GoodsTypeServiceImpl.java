package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.bcdm.foodtraceability.entity.GoodsType;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.GoodsTypeMapper;
import com.bcdm.foodtraceability.service.GoodsTypeService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;

import static com.bcdm.foodtraceability.common.Constants.*;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 商品类别服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Service
public class GoodsTypeServiceImpl extends ServiceImpl<GoodsTypeMapper, GoodsType> implements GoodsTypeService {

    @Override
    public List<GoodsType> getGoodsTypeList(Integer companyId) throws Exception {
        List<GoodsType> goodsTypeList = list(new QueryWrapper<GoodsType>().eq("company_id", companyId));
        if (SELECT_ZERO == goodsTypeList.size()) {
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, SELECT_GOODS_TYPE_INFO_FAIL);
        }
        return goodsTypeList;
    }


    @Override
    public Boolean createGoodsType(GoodsType goodsType) throws Exception {
        if (Boolean.FALSE.equals(checkGoodsType(goodsType, SELECT_CHECK_PARAM_CREATE))) {
            LocalDateTime now = LocalDateTime.now();
            goodsType.setUpdateTime(now);
            goodsType.setCreateTime(now);
            if (save(goodsType)) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, ADD_GOODS_TYPE_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, FIND_GOODS_TYPE_NAME_BY_COMPANY_FAIL2);
    }

    @Override
    public Boolean deleteGoodsType(GoodsType goodsType) throws Exception {
        if (Boolean.TRUE.equals(checkGoodsType(goodsType, SELECT_CHECK_PARAM_DELETE))) {
            if (removeById(goodsType.getGoodsTypeId())) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, DELETE_GOODS_TYPE_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, FIND_GOODS_TYPE_NAME_BY_COMPANY_FAIL1);
    }

    @Override
    public Boolean modifyGoodsType(GoodsType goodsType) throws Exception {
        if (Boolean.TRUE.equals(checkGoodsType(goodsType, SELECT_CHECK_PARAM_MODIFY))) {
            UpdateWrapper<GoodsType> goodsTypeQueryWrapper = new UpdateWrapper<>();
            goodsTypeQueryWrapper
                    .eq("company_id", goodsType.getCompanyId())
                    .eq("goods_type_id", goodsType.getGoodsTypeId())
                    .eq("update_time", goodsType.getUpdateTime())
                    .set("update_time", LocalDateTime.now())
                    .set("goods_type_name", goodsType.getGoodsTypeName());
            if (update(goodsTypeQueryWrapper)) {
                return true;
            }
            throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_GOODS_TYPE_FAIL);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, FIND_GOODS_TYPE_NAME_BY_COMPANY_FAIL1);
    }

    /**
     * 查询传入公司ID和商品种类的名称是否在该公司存在
     *
     * @param goodsType 希望操作的增删改查商品种类信息
     * @param selectId  操作ID create 1 modify 2 delete 3
     * @return 查询结果为0返回false，查询结果大于0返回true
     */
    private Boolean checkGoodsType(GoodsType goodsType, Integer selectId) {
        switch (selectId) {
            case 1:
            case 2:
                return SELECT_ZERO < count(new QueryWrapper<GoodsType>().eq("company_id", goodsType.getCompanyId()).eq("goods_type_name", goodsType.getGoodsTypeName()));
            case 3:
                return GET_ONE == count(new QueryWrapper<GoodsType>().eq("company_id", goodsType.getCompanyId()).eq("goods_type_id", goodsType.getGoodsTypeId()));
        }
        return null;
    }
}
