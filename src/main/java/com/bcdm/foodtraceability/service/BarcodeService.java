package com.bcdm.foodtraceability.service;

import com.bcdm.foodtraceability.entity.Barcode;
import com.baomidou.mybatisplus.extension.service.IService;

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
}
