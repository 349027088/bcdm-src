package com.bcdm.foodtraceability.controller;

import com.bcdm.foodtraceability.entity.GoodsType;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.service.GoodsTypeService;
import com.bcdm.foodtraceability.validatedgroup.CreateGroup;
import com.bcdm.foodtraceability.validatedgroup.DeleteGroup;
import com.bcdm.foodtraceability.validatedgroup.GetInfoGroup;
import com.bcdm.foodtraceability.validatedgroup.ModifyGroup;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
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
    public ReturnItem<Boolean> create(@Validated({CreateGroup.class})
                                      @RequestBody GoodsType goodsType) throws Exception {
        log.info("企业" + goodsType.getCompanyId() + "-----创建新商品种类:" + goodsType.getGoodsTypeName());
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(goodsTypeService.createGoodsType(goodsType));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(ADD_GOODS_TYPE_SUCCESS);
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
    public ReturnItem<Boolean> deleteGoodsTypeInfo(@Validated(DeleteGroup.class)
                                                   @RequestBody GoodsType goodsType) throws Exception {
        log.info("企业" + goodsType.getCompanyId() + "-----删除商品种类:" + goodsType.getGoodsTypeName());
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(goodsTypeService.deleteGoodsType(goodsType));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(DELETE_GOODS_TYPE_SUCCESS);
        return returnItem;
    }

    /**
     * 删除商品种类信息Controller
     *
     * @param goodsType 需要删除的商品种类
     * @return 删除状态
     * @throws Exception 删除失败
     */
    @PostMapping("/modify")
    @CrossOrigin
    public ReturnItem<Boolean> modifyGoodsTypeInfo(@Validated(ModifyGroup.class)
                                                   @RequestBody GoodsType goodsType) throws Exception {
        log.info("企业" + goodsType.getCompanyId() + "-----修改商品种类编号:" + goodsType.getGoodsTypeId() + "-----名称:" + goodsType.getGoodsTypeName());
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(goodsTypeService.modifyGoodsType(goodsType));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_GOODS_TYPE_SUCCESS);
        return returnItem;
    }

    /**
     * 获取公司的所有商品种类信息
     *
     * @param goodsType 装载用于查询所有商品种类的企业ID
     * @return 获取商品种类列表
     * @throws Exception 查询信息失败或者结果为0条信息
     */
    @PostMapping("/getGoodsTypeList")
    @CrossOrigin
    public ReturnItem<List<GoodsType>> getGoodsTypeList(@Validated({GetInfoGroup.class})
                                                        @RequestBody GoodsType goodsType) throws Exception {
        log.info("企业" + goodsType.getGoodsTypeId() + "-----获取所有商品种类信息");
        ReturnItem<List<GoodsType>> returnItem = new ReturnItem<>();
        returnItem.setT(goodsTypeService.getGoodsTypeList(goodsType.getCompanyId()));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(SELECT_GOODS_TYPE_INFO_SUCCESS);
        return returnItem;
    }

}

