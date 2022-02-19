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

import static com.bcdm.foodtraceability.common.Constants.GET_ONE;
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
    public String createBarcode(GoodsModel goodsModel) throws Exception {
        goodsModel.setBarcodeNumber(LocalDateTime.now().toLocalDate() + CreateUUID.getUUID() + goodsModel.getGoodsTableName().substring(goodsModel.getGoodsTableName().length() - 1));
        if (GET_ONE.equals(barcodeMapper.saveBarcode(goodsModel))) {
            return goodsModel.getBarcodeNumber();
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_BARCODE_FAIL);
    }

    @Override
    public GoodsModel getGoodsByQRCode(GoodsModel goodsModel) throws Exception {
        goodsModel.setCompanyId(Integer.parseInt(goodsModel.getBarcodeNumber().substring(goodsModel.getBarcodeNumber().length() - 1)));
        goodsModel.setGoodsModel();
        GoodsModel goodById = barcodeMapper.getGoodById(goodsModel);
        if (null != goodById) {
            return goodById;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, SELECT_GOODS_INFO_FAIL);
    }
}
