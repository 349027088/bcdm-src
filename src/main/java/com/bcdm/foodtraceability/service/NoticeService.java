package com.bcdm.foodtraceability.service;

import com.bcdm.foodtraceability.entity.Notice;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * <p>
 *  通知服务类
 * </p>
 *
 * @author 王
 * @since 2022-02-12
 */
public interface NoticeService extends IService<Notice> {

    /**
     * 创建新的通知
     *
     * @param notice 通知信息
     * @return 创建成功的通知信息
     * @throws Exception 创建新的通知失败
     */
    Boolean createNotice(Notice notice)throws Exception;
}
