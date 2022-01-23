package com.bcdm.foodtraceability.controller;


import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.GoodsType;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.entity.Supplier;
import com.bcdm.foodtraceability.service.GoodsTypeService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

import org.springframework.stereotype.Controller;

import java.util.List;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Controller
@RequestMapping("/goodsType")
public class GoodsTypeController {

    private final GoodsTypeService goodsTypeService;

    public GoodsTypeController(GoodsTypeService goodsTypeService) {
        this.goodsTypeService = goodsTypeService;
    }

    /**
     * 增加供应商信息Controller
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
     * 删除供应商信息Controller
     *
     * @param goodsType 用户账号密码和新密码
     * @return 修改成功的用户信息
     * @throws Exception 修改失败
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
     * 获取公司的所有未删除供应商列表信息
     *
     * @param company 需要获取供应商列表的企业
     * @return 获取供应商列表
     * @throws Exception 获取信息失败
     */
    @PostMapping("/getSupplierList")
    @CrossOrigin
    public ReturnItem<List<GoodsType>> getSupplierList(@RequestBody Company company) throws Exception {
        ReturnItem<List<GoodsType>> returnItem = new ReturnItem<>();
        returnItem.setT(goodsTypeService.getGoodsTypeList(company));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(SELECT_SUPPLIER_INFO_SUCCESS);
        return returnItem;
    }

}

