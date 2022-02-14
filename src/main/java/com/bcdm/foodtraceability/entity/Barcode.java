package com.bcdm.foodtraceability.entity;

import java.io.Serializable;

import com.bcdm.foodtraceability.validatedgroup.CreateGroup;
import com.bcdm.foodtraceability.validatedgroup.ModifyGroup;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotNull;

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
    @NotNull(message = "当前商品信息异常", groups = {CreateGroup.class, ModifyGroup.class})
    private Integer goodsId;


}
