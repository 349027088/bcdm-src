package com.bcdm.foodtraceability.controller;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.configuration.BlogAction;
import com.bcdm.foodtraceability.entity.GoodsType;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.entity.SelectPageEntity;
import com.bcdm.foodtraceability.service.GoodsTypeService;
import com.bcdm.foodtraceability.validatedgroup.CreateGroup;
import com.bcdm.foodtraceability.validatedgroup.DeleteGroup;
import com.bcdm.foodtraceability.validatedgroup.GetInfoGroup;
import com.bcdm.foodtraceability.validatedgroup.ModifyGroup;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

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
public class GoodsTypeController {

    private final GoodsTypeService goodsTypeService;

    public GoodsTypeController(GoodsTypeService goodsTypeService) {
        this.goodsTypeService = goodsTypeService;
    }

    /**
     * 获取公司的所有商品种类信息
     *
     * @param selectInfo 装载用于查询所有商品种类的企业ID,查询条件
     * @return 获取商品种类列表
     * @throws Exception 查询信息失败或者结果为0条信息
     */
    @PostMapping("/getGoodsTypeList")
    @CrossOrigin
    public ReturnItem<IPage<GoodsType>> getGoodsTypeList(@RequestBody String selectInfo) throws Exception {
        SelectPageEntity<GoodsType> selectPageEntity = new SelectPageEntity<>(selectInfo);
        BlogAction.logger.info("企业:" + selectPageEntity.getCompanyId() + "-----获取所有商品种类信息");
        ReturnItem<IPage<GoodsType>> returnItem = new ReturnItem<>();
        returnItem.setT(goodsTypeService.getGoodsTypeList(selectPageEntity));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(SELECT_GOODS_TYPE_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 获取公司的指定商品种类信息
     *
     * @param getOneInfo 获取公司的指定商品种类的Id和公司Id
     * @return 指定ID的商品种类信息
     * @throws Exception 查询失败
     */
    @PostMapping("/getGoodsTypeById")
    @CrossOrigin
    public ReturnItem<GoodsType> getGoodsTypeById(@Validated({GetInfoGroup.class})
                                                      @RequestBody GoodsType getOneInfo) throws Exception {
        BlogAction.logger.info("企业:" + getOneInfo.getCompanyId() + "-----获取编号:" + getOneInfo.getGoodsTypeId() + "的商品种类信息");
        ReturnItem<GoodsType> returnItem = new ReturnItem<>();
        returnItem.setT(goodsTypeService.getGoodsTypeById(getOneInfo));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(SELECT_GOODS_TYPE_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 增加商品种类信息Controller
     *
     * @param goodsType 需要添加的商品种类信息
     * @return true 创建成功
     * @throws Exception 创建失败
     */
    @PostMapping("/create")
    @CrossOrigin
    public ReturnItem<Boolean> create(@Validated({CreateGroup.class})
                                      @RequestBody GoodsType goodsType) throws Exception {
        BlogAction.logger.info("企业:" + goodsType.getCompanyId() + "-----创建新商品种类:" + goodsType.getGoodsTypeName());
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(goodsTypeService.createGoodsType(goodsType));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(ADD_GOODS_TYPE_SUCCESS);
        return returnItem;
    }

    /**
     * 修改商品种类信息Controller
     *
     * @param goodsType 需要删除的商品种类
     * @return true 修改成功
     * @throws Exception 修改失败
     */
    @PostMapping("/modify")
    @CrossOrigin
    public ReturnItem<Boolean> modifyGoodsTypeInfo(@Validated(ModifyGroup.class)
                                                   @RequestBody GoodsType goodsType) throws Exception {
        BlogAction.logger.info("企业:" + goodsType.getCompanyId() + "-----修改商品种类编号:" + goodsType.getGoodsTypeId() + "-----名称:" + goodsType.getGoodsTypeName());
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(goodsTypeService.modifyGoodsType(goodsType));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_GOODS_TYPE_SUCCESS);
        return returnItem;
    }

    /**
     * 删除商品种类信息Controller
     *
     * @param goodsType 需要删除的商品种类
     * @return true 删除成功
     * @throws Exception 删除失败
     */
    @PostMapping("/delete")
    @CrossOrigin
    public ReturnItem<Boolean> deleteGoodsTypeInfo(@Validated(DeleteGroup.class)
                                                   @RequestBody GoodsType goodsType) throws Exception {
        BlogAction.logger.info("企业:" + goodsType.getCompanyId() + "-----删除商品种类:" + goodsType.getGoodsTypeName());
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(goodsTypeService.deleteGoodsType(goodsType));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(DELETE_GOODS_TYPE_SUCCESS);
        return returnItem;
    }

}

