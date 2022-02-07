package com.bcdm.foodtraceability.controller;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.Goods;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.service.GoodsService;
import org.springframework.web.bind.annotation.*;


import java.util.List;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 *  商品前端控制器
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@RestController
@RequestMapping("/goods")
public class GoodsController {

    private final GoodsService goodsService;

    public GoodsController(GoodsService goodsService) {
        this.goodsService = goodsService;
    }


    /**
     * 获取公司的所有商品种类信息
     *
     * @param company 需要获取商品种类列表的企业
     * @return 获取商品种类列表
     */
    @PostMapping("/getGoodsList")
    @CrossOrigin
    public ReturnItem<IPage<Goods>> getGoodsTypeList(@RequestBody Company company)throws Exception{
        ReturnItem<IPage<Goods>> returnItem = new ReturnItem<>();
        returnItem.setT(goodsService.getGoodsListByCompany(company.getCompanyId()));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(SELECT_GOODS_INFO_SUCCESS);
        return returnItem;
    }


    /**
     * 增加商品种类信息Controller
     *
     * @param goods 需要添加的供应想信息
     * @return true 创建成功
     * @throws Exception 增加供应商失败
     */
    @PostMapping("/create")
    @CrossOrigin
    public ReturnItem<Boolean> create(@RequestBody Goods goods) throws Exception {
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
    public ReturnItem<Boolean> modify(@RequestBody Goods goods) throws Exception {
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
    public ReturnItem<Boolean> delete(@RequestBody Goods goods) throws Exception {
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(goodsService.deleteGoods(goods));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(DELETE_GOODS_INFO_SUCCESS);
        return returnItem;
    }

}

