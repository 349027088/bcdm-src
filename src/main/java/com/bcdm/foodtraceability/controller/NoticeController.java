package com.bcdm.foodtraceability.controller;


import com.bcdm.foodtraceability.entity.Notice;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.service.NoticeService;
import com.bcdm.foodtraceability.validatedgroup.CreateGroup;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.CREATE_NOTICE_SUCCESS;

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
@Slf4j
public class NoticeController {

    private final NoticeService noticeService;

    public NoticeController(NoticeService noticeService) {
        this.noticeService = noticeService;
    }

    /**
     * 通知生成Controller
     *
     * @return 创建结果
     * @throws Exception 生成新的通知失败
     */
    @PostMapping("/create")
    @CrossOrigin
    public ReturnItem<Boolean> create(@Validated({CreateGroup.class})
                                      @RequestBody Notice notice) throws Exception {
        log.info("企业：" + notice.getCompanyId() + "----发布新的通知");
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(noticeService.createNotice(notice));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(CREATE_NOTICE_SUCCESS);
        return returnItem;
    }

}

