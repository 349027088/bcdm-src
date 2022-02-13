package com.bcdm.foodtraceability.entity;

import java.io.Serializable;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

/**
 * <p>
 * 商品二维码信息载体
 * </p>
 *
 * @author 王
 * @since 2022-02-12
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = false)
public class Barcode implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 条形码编号
     */
    private String barcodeNumber;

    /**
     * 商品编号
     */
    private Integer goodsId;


}
