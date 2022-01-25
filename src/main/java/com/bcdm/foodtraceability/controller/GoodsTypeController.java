package com.bcdm.foodtraceability.controller;


import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.GoodsType;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.service.GoodsTypeService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;


import java.util.List;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 商品种类前端控制器
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@RestController
@RequestMapping("/goodsType")
@Slf4j
public class GoodsTypeController {

    private final GoodsTypeService goodsTypeService;

    public GoodsTypeController(GoodsTypeService goodsTypeService) {
        this.goodsTypeService = goodsTypeService;
    }

    /**
     * 增加商品种类信息Controller
     *
     * @param goodsType 需要添加的供应想信息
     * @return 创建成功的供应商信息
     * @throws Exception 增加供应商失败
     */
    @PostMapping("/create")
    @CrossOrigin
    public ReturnItem<Boolean> create(@RequestBody GoodsType goodsType) throws Exception {
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(goodsTypeService.createGoodsType(goodsType));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(ADD_SUPPLIER_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 删除商品种类信息Controller
     *
     * @param goodsType 需要删除的商品种类
     * @return 删除状态
     * @throws Exception 删除失败
     */
    @PostMapping("/delete")
    @CrossOrigin
    public ReturnItem<Boolean> delete(@RequestBody GoodsType goodsType) throws Exception {
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(goodsTypeService.deleteGoodsType(goodsType));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(DELETE_SUPPLIER_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 获取公司的所有商品种类信息
     *
     * @param company 需要获取供应商列表的企业
     * @return 获取商品种类列表
     */
    @PostMapping("/getGoodsTypeList")
    @CrossOrigin
    public ReturnItem<List<GoodsType>> getGoodsTypeList(@RequestBody Company company){
        log.info(company.toString());
        ReturnItem<List<GoodsType>> returnItem = new ReturnItem<>();
        returnItem.setT(goodsTypeService.getGoodsTypeList(company));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(SELECT_SUPPLIER_INFO_SUCCESS);
        return returnItem;
    }

}

