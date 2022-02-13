package com.bcdm.foodtraceability.service.impl;

import com.bcdm.foodtraceability.entity.Notice;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.NoticeMapper;
import com.bcdm.foodtraceability.service.NoticeService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.CREATE_BARCODE_FAIL;

/**
 * <p>
 *  通知服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-02-12
 */
@Service
public class NoticeServiceImpl extends ServiceImpl<NoticeMapper, Notice> implements NoticeService {

    @Override
    public Boolean createNotice(Notice notice) throws Exception {
        LocalDateTime now = LocalDateTime.now();
        notice.setCreateTime(now);
        notice.setUpdateTime(now);
        if (save(notice)){
            return true;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_BARCODE_FAIL);
    }
}
