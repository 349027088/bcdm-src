package com.bcdm.foodtraceability.controller;

import com.bcdm.foodtraceability.entity.Barcode;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.service.BarcodeService;
import com.bcdm.foodtraceability.validatedgroup.CreateGroup;
import com.bcdm.foodtraceability.validatedgroup.RegisterGroup;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.CREATE_BARCODE_SUCCESS;

/**
 * <p>
 * 商品二维码前端控制器X
 * </p>
 *
 * @author 王
 * @since 2022-02-12
 */
@RestController
@RequestMapping("/barcode")
@Slf4j
public class BarcodeController {

    private final BarcodeService barcodeService;

    public BarcodeController(BarcodeService barcodeService) {
        this.barcodeService = barcodeService;
    }

    /**
     * 商品二维码生成Controller
     *
     * @return 生成的二维码内置码
     * @throws Exception 生成二维码信息失败
     */
    @PostMapping("/create")
    @CrossOrigin
    public ReturnItem<Barcode> create(@Validated({CreateGroup.class})
                                          @RequestBody Barcode barcode) throws Exception {
        log.info("商品编号：" + barcode.getGoodsId() + "----生成新的二维码");
        ReturnItem<Barcode> returnItem = new ReturnItem<>();
        returnItem.setT(barcodeService.createBarcode(barcode.getGoodsId()));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(CREATE_BARCODE_SUCCESS);
        return returnItem;
    }

}

