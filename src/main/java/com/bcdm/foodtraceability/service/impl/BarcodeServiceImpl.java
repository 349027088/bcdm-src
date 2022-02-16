package com.bcdm.foodtraceability.service.impl;

import com.bcdm.foodtraceability.common.CreateUUID;
import com.bcdm.foodtraceability.entity.Barcode;
import com.bcdm.foodtraceability.entity.GoodsModel;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.BarcodeMapper;
import com.bcdm.foodtraceability.service.BarcodeService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.CREATE_BARCODE_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.SELECT_GOODS_INFO_FAIL;

/**
 * <p>
 * 商品二维码服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-02-12
 */
@Service
public class BarcodeServiceImpl extends ServiceImpl<BarcodeMapper, Barcode> implements BarcodeService {

    private final BarcodeMapper barcodeMapper;

    public BarcodeServiceImpl(BarcodeMapper barcodeMapper) {
        this.barcodeMapper = barcodeMapper;
    }

    @Override
    public Barcode createBarcode(Integer goodsId) throws Exception {
        Barcode barcode = new Barcode();
        barcode.setBarcodeNumber(LocalDateTime.now() + CreateUUID.getUUID());
        barcode.setGoodsId(goodsId);
        if (save(barcode)) {
            return barcode;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_BARCODE_FAIL);
    }

    @Override
    public GoodsModel getGoodsByQRCode(Barcode barcode) throws Exception {
        GoodsModel goodById = barcodeMapper.getGoodById(barcode);
        if (null != goodById) {
            return goodById;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, SELECT_GOODS_INFO_FAIL);
    }
}
