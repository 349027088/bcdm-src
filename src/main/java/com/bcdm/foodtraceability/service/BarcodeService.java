package com.bcdm.foodtraceability.service;

import com.bcdm.foodtraceability.entity.Barcode;
import com.baomidou.mybatisplus.extension.service.IService;
import com.bcdm.foodtraceability.entity.GoodsModel;

/**
 * <p>
 * 商品二维码服务类
 * </p>
 *
 * @author 王
 * @since 2022-02-12
 */
public interface BarcodeService extends IService<Barcode> {

    /**
     * 创建商品二维码
     *
     * @param goodsId 需要创建二维码的商品
     * @return 创建成功额商品二维码编号
     * @throws Exception 创建二维码失败
     */
    Barcode createBarcode(Integer goodsId) throws Exception;

    /**
     * 查询指定的商品信息
     *
     * @param barcode 需要查询的商品二维码
     * @return 查询到的商品信息
     * @throws Exception 查询商品信息失败
     */
    GoodsModel getGoodsByQRCode(Barcode barcode) throws Exception;
}
