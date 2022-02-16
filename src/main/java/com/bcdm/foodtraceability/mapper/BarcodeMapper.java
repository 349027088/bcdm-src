package com.bcdm.foodtraceability.mapper;

import com.bcdm.foodtraceability.entity.Barcode;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.bcdm.foodtraceability.entity.GoodsModel;

/**
 * <p>
 *  商品二维码Mapper 接口
 * </p>
 *
 * @author 王
 * @since 2022-02-12
 */
public interface BarcodeMapper extends BaseMapper<Barcode> {

    /**
     * 参数条件查询
     *
     * @param barcode       分页条件
     * @return 分页后的商品信息
     */
    GoodsModel getGoodById(Barcode barcode);

}
