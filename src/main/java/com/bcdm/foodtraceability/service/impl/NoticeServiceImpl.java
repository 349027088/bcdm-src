package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.bcdm.foodtraceability.entity.Notice;
import com.bcdm.foodtraceability.entity.SelectPageEntity;
import com.bcdm.foodtraceability.entity.UserModel;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.NoticeMapper;
import com.bcdm.foodtraceability.service.NoticeService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

import static com.bcdm.foodtraceability.common.Constants.*;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 通知服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-02-12
 */
@Service
public class NoticeServiceImpl extends ServiceImpl<NoticeMapper, Notice> implements NoticeService {

    @Override
    public Boolean companyCreate(Notice notice) throws Exception {
        LocalDateTime now = LocalDateTime.now();
        notice.setCreateTime(now);
        notice.setUpdateTime(now);
        notice.setNoticeLevel(NOTICE_COMPANY);
        if (save(notice)) {
            return true;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_NOTICE_FAIL);
    }

    @Override
    public Boolean managementCreate(Notice notice) throws Exception {
        LocalDateTime now = LocalDateTime.now();
        notice.setCreateTime(now);
        notice.setUpdateTime(now);
        notice.setNoticeLevel(NOTICE_SYSTEM);
        if (save(notice)) {
            return true;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, CREATE_NOTICE_FAIL);
    }

    @Override
    public Long getNewNotice(UserModel userModel) throws Exception {
        QueryWrapper<Notice> noticeQueryWrapper = new QueryWrapper<>();
        noticeQueryWrapper
                .ge("end_time", LocalDateTime.now())
                .gt("update_time", userModel.getNoticeCheck());
            return count(noticeQueryWrapper);
    }

    @Override
    public IPage<Notice> getAllNotice(SelectPageEntity<Notice> selectInfo) throws Exception {
        QueryWrapper<Notice> noticeQueryWrapper = new QueryWrapper<>();
        noticeQueryWrapper
                .eq("company_id", selectInfo.getCompanyId())
                .or()
                .eq("company_id", NOTICE_SYSTEM)
                .gt("end_time", LocalDateTime.now());
        IPage<Notice> page = page(selectInfo.getPageInfo(), noticeQueryWrapper);
        if (SELECT_ZERO != page.getTotal()) {
            return page;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, GET_NOTICE_FAIL);
    }

    @Override
    public Boolean modifyNotice(Notice notice) throws Exception {
        UpdateWrapper<Notice> noticeUpdateWrapper = new UpdateWrapper<>();
        LocalDateTime now = LocalDateTime.now();
        noticeUpdateWrapper
                .eq("notice_id", notice.getNoticeId())
                .eq("company_id", notice.getCompanyId())
                .eq("notice_level", notice.getNoticeLevel())
                .eq("update_time", notice.getUpdateTime())
                .set("notice_title", notice.getNoticeTitle())
                .set("notice_info", notice.getNoticeInfo())
                .set("end_time", notice.getEndTime())
                .set("user_name", notice.getUserName())
                .set("update_time", now);
        if (update(noticeUpdateWrapper)) {
            return true;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_NOTICE_FAIL);
    }

    @Override
    public Boolean deleteNotice(Notice notice) throws Exception {
        Notice noticeById = getById(notice.getNoticeId());
        if (null != noticeById && noticeById.getCompanyId().equals(notice.getCompanyId()) && remove(new QueryWrapper<Notice>().eq("notice_id", notice.getNoticeId()))) {
            return true;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, DELETE_NOTICE_FAIL);
    }
}
