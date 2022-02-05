package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.bcdm.foodtraceability.common.CreateUUID;
import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.Goods;
import com.bcdm.foodtraceability.entity.GoodsType;
import com.bcdm.foodtraceability.mapper.GoodsMapper;
import com.bcdm.foodtraceability.service.GoodsService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.bcdm.foodtraceability.service.GoodsTypeService;
import com.bcdm.foodtraceability.service.IconService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.time.LocalDateTime;
import java.util.List;

import static com.baomidou.mybatisplus.core.enums.SqlMethod.SELECT_ONE;
import static com.bcdm.foodtraceability.common.Constants.*;

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
    public List<Goods> getGoodsListByCompany(Integer companyId) throws Exception {
        return list(new QueryWrapper<Goods>().eq("company_id", companyId));
    }

    @Override
    public List<Goods> getGoodsListByStatus(Integer status) throws Exception {
        return null;
    }

    @Override
    public List<Goods> getGoodsListBySupplier(Integer companyId, Integer supplierId) throws Exception {
        return list(new QueryWrapper<Goods>().eq("company_id", companyId).eq("supplier_id",supplierId));
    }

    @Override
    public List<Goods> getGoodsListByGoodsType(Integer companyId, Integer goodsTypeId) throws Exception {
        return list(new QueryWrapper<Goods>().eq("company_id", companyId).eq("goods_type_id",goodsTypeId));
    }

    @Override
    public Boolean createGoods(Goods goods) throws Exception {
        LocalDateTime now = LocalDateTime.now();
        goods.setGoodsLevel(GOODS_LEVEL_ZERO);
        goods.setGoodsStatus(GOODS_STATUS_ON_SERVICE);
        goods.setCreateTime(now);
        goods.setUpdateTime(now);
        goods.setBarcodeNumber(CreateUUID.getUUID());
        log.info(goods.toString());
        return save(goods);
//        return INSERT_ONE == goodsMapper.insertNewGoods(goods);
    }

    @Override
    public List<Goods> createGoodsList(Company company, List<Goods> goodsList) throws Exception {
        return null;
    }

    @Override
    public List<Goods> modifyGoodsList(Company company, List<Goods> goodsList) throws Exception {
        return null;
    }

    @Override
    public Boolean modifyGoods(Goods goods) throws Exception {
        return null;
    }

    @Override
    public boolean deleteGoods(Goods goods) throws Exception {
        return removeById(goods);
    }

    @Override
    public int deleteGoodsList(Company company, List<Goods> goodsList) throws Exception {
        return 0;
    }
}
