package com.bcdm.foodtraceability.service.impl;

import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.Goods;
import com.bcdm.foodtraceability.entity.GoodsType;
import com.bcdm.foodtraceability.mapper.GoodsMapper;
import com.bcdm.foodtraceability.service.GoodsService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.bcdm.foodtraceability.service.GoodsTypeService;
import com.bcdm.foodtraceability.service.IconService;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * <p>
 *  商品服务实现类
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

    @Override
    public List<Goods> getGoodsListByCompany(Company company) throws Exception {
        return null;
    }

    @Override
    public List<Goods> getGoodsListByStatus(Integer status) throws Exception {
        return null;
    }

    @Override
    public List<Goods> getGoodsListByGoodsType(Company company, GoodsType goodsType) throws Exception {
        return null;
    }

    @Override
    public Goods createGoods(Company company, Goods goods) throws Exception {
        return null;
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
    public Goods modifyGoods(Company company, Goods goods, MultipartFile icon) throws Exception {
        return null;
    }

    @Override
    public boolean deleteGoods(Company company, Goods goods) throws Exception {
        return false;
    }

    @Override
    public int deleteGoodsList(Company company, List<Goods> goodsList) throws Exception {
        return 0;
    }
}
