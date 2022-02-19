package com.bcdm.foodtraceability.controller;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.configuration.BlogAction;
import com.bcdm.foodtraceability.entity.*;
import com.bcdm.foodtraceability.service.NoticeService;
import com.bcdm.foodtraceability.validatedgroup.CreateGroup;
import com.bcdm.foodtraceability.validatedgroup.DeleteGroup;
import com.bcdm.foodtraceability.validatedgroup.ModifyGroup;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;


import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 通知前端控制器
 * </p>
 *
 * @author 王
 * @since 2022-02-12
 */
@RestController
@RequestMapping("/notice")
public class NoticeController {

    private final NoticeService noticeService;

    public NoticeController(NoticeService noticeService) {
        this.noticeService = noticeService;
    }

    /**
     * 公司通知生成Controller
     *
     * @return 创建结果
     * @throws Exception 生成新的通知失败
     */
    @PostMapping("/companyCreate")
    @CrossOrigin
    public ReturnItem<Boolean> companyCreate(@Validated({CreateGroup.class})
                                             @RequestBody Notice notice) throws Exception {
        BlogAction.logger.info("企业：" + notice.getCompanyId() + "----发布新的通知");
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(noticeService.companyCreate(notice));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(CREATE_NOTICE_SUCCESS);
        return returnItem;
    }

    /**
     * 系统通知生成Controller
     *
     * @return 创建结果
     * @throws Exception 生成新的通知失败
     */
    @PostMapping("/managementCreate")
    @CrossOrigin
    public ReturnItem<Boolean> managementCreate(@Validated({CreateGroup.class})
                                                @RequestBody Notice notice) throws Exception {
        BlogAction.logger.info("管理员：" + notice.getUserName() + "----发布新的通知");
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(noticeService.managementCreate(notice));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(CREATE_NOTICE_SUCCESS);
        return returnItem;
    }

    /**
     * 通知修改Controller
     *
     * @return 创建结果
     * @throws Exception 生成新的通知失败
     */
    @PostMapping("/modifyNotice")
    @CrossOrigin
    public ReturnItem<Boolean> modify(@Validated({ModifyGroup.class})
                                      @RequestBody Notice notice) throws Exception {
        BlogAction.logger.info("企业：" + notice.getCompanyId() + "用户：" + notice.getUserName() + "----修改通知");
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(noticeService.modifyNotice(notice));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_NOTICE_SUCCESS);
        return returnItem;
    }

    /**
     * 通知生成Controller
     *
     * @return 创建结果
     * @throws Exception 生成新的通知失败
     */
    @PostMapping("/getAllNotice")
    @CrossOrigin
    public ReturnItem<IPage<Notice>> getAllNotice(@RequestBody String selectInfo) throws Exception {
        SelectPageEntity<Notice> selectPageEntity = new SelectPageEntity<>(selectInfo);
        BlogAction.logger.info("用户：" + selectPageEntity.getUserId() + "----获取历史通知");
        ReturnItem<IPage<Notice>> returnItem = new ReturnItem<>();
        returnItem.setT(noticeService.getAllNotice(selectPageEntity));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(SELECT_NOTICE_SUCCESS);
        return returnItem;
    }

    /**
     * 通知生成Controller
     *
     * @return 创建结果
     * @throws Exception 生成新的通知失败
     */
    @PostMapping("/getNewNotice")
    @CrossOrigin
    public ReturnItem<Long> getNewNotice(@RequestBody UserModel userModel) throws Exception {
        BlogAction.logger.info("用户:" + userModel.getUserId() + "----获取新通知");
        ReturnItem<Long> returnItem = new ReturnItem<>();
        returnItem.setT(noticeService.getNewNotice(userModel));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(SELECT_NOTICE_SUCCESS);
        return returnItem;
    }

    /**
     * 通知生成Controller
     *
     * @return 创建结果
     * @throws Exception 生成新的通知失败
     */
    @PostMapping("/deleteNotice")
    @CrossOrigin
    public ReturnItem<Boolean> deleteNotice(@Validated({DeleteGroup.class})
                                            @RequestBody Notice notice) throws Exception {
        BlogAction.logger.info("企业:" + notice.getCompanyId() + "----删除编号" + notice.getNoticeId() + "通知");
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(noticeService.deleteNotice(notice));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(DELETE_NOTICE_SUCCESS);
        return returnItem;
    }

}

