package com.bcdm.foodtraceability.controller;


import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.configuration.BlogAction;
import com.bcdm.foodtraceability.entity.*;
import com.bcdm.foodtraceability.service.GoodsService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 商品前端控制器
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@RestController
@RequestMapping("/goods")
@Slf4j
public class GoodsController {

    private final GoodsService goodsService;

    public GoodsController(GoodsService goodsService) {
        this.goodsService = goodsService;
    }

    /**
     * 获取公司的所有商品信息
     *
     * @param selectInfo 需要获取商品列表的企业
     * @return 获取商品列表
     */
    @PostMapping("/getGoodsList")
    @CrossOrigin
    public ReturnItem<IPage<GoodsModel>> getGoodsTypeList(@RequestBody String selectInfo) throws Exception {
        SelectPageEntity<GoodsModel> selectPageEntity = new SelectPageEntity<>(selectInfo);
        JSONObject selectPageInfo = JSONObject.parseObject(selectInfo);
        selectPageEntity.setSelectInfo(selectPageInfo.getObject("goods", GoodsModel.class));
        BlogAction.logger.info("企业:" + selectPageEntity.getCompanyId() + "-----获取商品信息");
        ReturnItem<IPage<GoodsModel>> returnItem = new ReturnItem<>();
        returnItem.setT(goodsService.getGoodsListByCompany(selectPageEntity));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(SELECT_GOODS_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 增加商品信息Controller
     *
     * @param goods 需要添加的商品信息
     * @return true 创建成功
     * @throws Exception 增加商品信息失败
     */
    @PostMapping("/create")
    @CrossOrigin
    public ReturnItem<Boolean> create(@RequestBody GoodsModel goods) throws Exception {
        BlogAction.logger.info("企业:" + goods.getCompanyId() + "-----添加" + goods.getGoodsName() + "商品信息");
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(goodsService.createGoods(goods));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(ADD_GOODS_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 修改商品信息Controller
     *
     * @param goods 需要修改的商品信息
     * @return true 更新状态
     * @throws Exception 更新失败
     */
    @PostMapping("/modify")
    @CrossOrigin
    public ReturnItem<Boolean> modify(@RequestBody GoodsModel goods) throws Exception {
        BlogAction.logger.info("企业:" + goods.getCompanyId() + "-----修改编号" + goods.getGoodsId() + "商品信息" );
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(goodsService.modifyGoods(goods));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_GOODS_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 删除商品信息Controller
     *
     * @param goods 需要删除的商品信息
     * @return true 删除成功
     * @throws Exception 删除失败
     */
    @PostMapping("/delete")
    @CrossOrigin
    public ReturnItem<Boolean> delete(@RequestBody GoodsModel goods) throws Exception {
        BlogAction.logger.info("企业:" + goods.getCompanyId() + "-----删除编号" + goods.getGoodsId() + "商品信息" );
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(goodsService.deleteGoods(goods));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(DELETE_GOODS_INFO_SUCCESS);
        return returnItem;
    }

}

