
:- module(number_words,
          [number_word/2]).

number_word(N, W, english) :- number_word(N, W). 

number_word(1, [one]).
number_word(2, [two]).
number_word(3, [three]).
number_word(4, [four]).
number_word(5, [five]).
number_word(6, [six]).
number_word(7, [seven]).
number_word(8, [eight]).
number_word(9, [nine]).
number_word(10, [ten]).
number_word(11, [eleven]).
number_word(12, [twelve]).
number_word(13, [thir, teen]).
number_word(14, [four, teen]).
number_word(15, [fif, teen]).
number_word(16, [six, teen]).
number_word(17, [seven, teen]).
number_word(18, [eight, teen]).
number_word(19, [nine, teen]).
number_word(20, [twen, ty]).
number_word(21, [twen, ty,one]).
number_word(22, [twen, ty,two]).
number_word(23, [twen, ty,three]).
number_word(24, [twen, ty,four]).
number_word(25, [twen, ty,five]).
number_word(26, [twen, ty,six]).
number_word(27, [twen, ty,seven]).
number_word(28, [twen, ty,eight]).
number_word(29, [twen, ty,nine]).
number_word(30, [thir, ty]).
number_word(31, [thir, ty,one]).
number_word(32, [thir, ty,two]).
number_word(33, [thir, ty,three]).
number_word(34, [thir, ty,four]).
number_word(35, [thir, ty,five]).
number_word(36, [thir, ty,six]).
number_word(37, [thir, ty,seven]).
number_word(38, [thir, ty,eight]).
number_word(39, [thir, ty,nine]).
number_word(40, [four, ty]).
number_word(41, [four, ty,one]).
number_word(42, [four, ty,two]).
number_word(43, [four, ty,three]).
number_word(44, [four, ty,four]).
number_word(45, [four, ty,five]).
number_word(46, [four, ty,six]).
number_word(47, [four, ty,seven]).
number_word(48, [four, ty,eight]).
number_word(49, [four, ty,nine]).
number_word(50, [fif, ty]).
number_word(51, [fif, ty,one]).
number_word(52, [fif, ty,two]).
number_word(53, [fif, ty,three]).
number_word(54, [fif, ty,four]).
number_word(55, [fif, ty,five]).
number_word(56, [fif, ty,six]).
number_word(57, [fif, ty,seven]).
number_word(58, [fif, ty,eight]).
number_word(59, [fif, ty,nine]).
number_word(60, [six, ty]).
number_word(61, [six, ty,one]).
number_word(62, [six, ty,two]).
number_word(63, [six, ty,three]).
number_word(64, [six, ty,four]).
number_word(65, [six, ty,five]).
number_word(66, [six, ty,six]).
number_word(67, [six, ty,seven]).
number_word(68, [six, ty,eight]).
number_word(69, [six, ty,nine]).
number_word(70, [seven, ty]).
number_word(71, [seven, ty,one]).
number_word(72, [seven, ty,two]).
number_word(73, [seven, ty,three]).
number_word(74, [seven, ty,four]).
number_word(75, [seven, ty,five]).
number_word(76, [seven, ty,six]).
number_word(77, [seven, ty,seven]).
number_word(78, [seven, ty,eight]).
number_word(79, [seven, ty,nine]).
number_word(80, [eight, ty]).
number_word(81, [eight, ty,one]).
number_word(82, [eight, ty,two]).
number_word(83, [eight, ty,three]).
number_word(84, [eight, ty,four]).
number_word(85, [eight, ty,five]).
number_word(86, [eight, ty,six]).
number_word(87, [eight, ty,seven]).
number_word(88, [eight, ty,eight]).
number_word(89, [eight, ty,nine]).
number_word(90, [nine, ty]).
number_word(91, [nine, ty,one]).
number_word(92, [nine, ty,two]).
number_word(93, [nine, ty,three]).
number_word(94, [nine, ty,four]).
number_word(95, [nine, ty,five]).
number_word(96, [nine, ty,six]).
number_word(97, [nine, ty,seven]).
number_word(98, [nine, ty,eight]).
number_word(99, [nine, ty,nine]).
number_word(100, [one,hundred]).
number_word(101, [one,hundred,one]).
number_word(102, [one,hundred,two]).
number_word(103, [one,hundred,three]).
number_word(104, [one,hundred,four]).
number_word(105, [one,hundred,five]).
number_word(106, [one,hundred,six]).
number_word(107, [one,hundred,seven]).
number_word(108, [one,hundred,eight]).
number_word(109, [one,hundred,nine]).
number_word(110, [one,hundred,ten]).
number_word(111, [one,hundred,eleven]).
number_word(112, [one,hundred,twelve]).
number_word(113, [one,hundred,thir, teen]).
number_word(114, [one,hundred,four, teen]).
number_word(115, [one,hundred,fif, teen]).
number_word(116, [one,hundred,six, teen]).
number_word(117, [one,hundred,seven, teen]).
number_word(118, [one,hundred,eight, teen]).
number_word(119, [one,hundred,nine, teen]).
number_word(120, [one,hundred,twen, ty]).
number_word(121, [one,hundred,twen, ty,one]).
number_word(122, [one,hundred,twen, ty,two]).
number_word(123, [one,hundred,twen, ty,three]).
number_word(124, [one,hundred,twen, ty,four]).
number_word(125, [one,hundred,twen, ty,five]).
number_word(126, [one,hundred,twen, ty,six]).
number_word(127, [one,hundred,twen, ty,seven]).
number_word(128, [one,hundred,twen, ty,eight]).
number_word(129, [one,hundred,twen, ty,nine]).
number_word(130, [one,hundred,thir, ty]).
number_word(131, [one,hundred,thir, ty,one]).
number_word(132, [one,hundred,thir, ty,two]).
number_word(133, [one,hundred,thir, ty,three]).
number_word(134, [one,hundred,thir, ty,four]).
number_word(135, [one,hundred,thir, ty,five]).
number_word(136, [one,hundred,thir, ty,six]).
number_word(137, [one,hundred,thir, ty,seven]).
number_word(138, [one,hundred,thir, ty,eight]).
number_word(139, [one,hundred,thir, ty,nine]).
number_word(140, [one,hundred,four, ty]).
number_word(141, [one,hundred,four, ty,one]).
number_word(142, [one,hundred,four, ty,two]).
number_word(143, [one,hundred,four, ty,three]).
number_word(144, [one,hundred,four, ty,four]).
number_word(145, [one,hundred,four, ty,five]).
number_word(146, [one,hundred,four, ty,six]).
number_word(147, [one,hundred,four, ty,seven]).
number_word(148, [one,hundred,four, ty,eight]).
number_word(149, [one,hundred,four, ty,nine]).
number_word(150, [one,hundred,fif, ty]).
number_word(151, [one,hundred,fif, ty,one]).
number_word(152, [one,hundred,fif, ty,two]).
number_word(153, [one,hundred,fif, ty,three]).
number_word(154, [one,hundred,fif, ty,four]).
number_word(155, [one,hundred,fif, ty,five]).
number_word(156, [one,hundred,fif, ty,six]).
number_word(157, [one,hundred,fif, ty,seven]).
number_word(158, [one,hundred,fif, ty,eight]).
number_word(159, [one,hundred,fif, ty,nine]).
number_word(160, [one,hundred,six, ty]).
number_word(161, [one,hundred,six, ty,one]).
number_word(162, [one,hundred,six, ty,two]).
number_word(163, [one,hundred,six, ty,three]).
number_word(164, [one,hundred,six, ty,four]).
number_word(165, [one,hundred,six, ty,five]).
number_word(166, [one,hundred,six, ty,six]).
number_word(167, [one,hundred,six, ty,seven]).
number_word(168, [one,hundred,six, ty,eight]).
number_word(169, [one,hundred,six, ty,nine]).
number_word(170, [one,hundred,seven, ty]).
number_word(171, [one,hundred,seven, ty,one]).
number_word(172, [one,hundred,seven, ty,two]).
number_word(173, [one,hundred,seven, ty,three]).
number_word(174, [one,hundred,seven, ty,four]).
number_word(175, [one,hundred,seven, ty,five]).
number_word(176, [one,hundred,seven, ty,six]).
number_word(177, [one,hundred,seven, ty,seven]).
number_word(178, [one,hundred,seven, ty,eight]).
number_word(179, [one,hundred,seven, ty,nine]).
number_word(180, [one,hundred,eight, ty]).
number_word(181, [one,hundred,eight, ty,one]).
number_word(182, [one,hundred,eight, ty,two]).
number_word(183, [one,hundred,eight, ty,three]).
number_word(184, [one,hundred,eight, ty,four]).
number_word(185, [one,hundred,eight, ty,five]).
number_word(186, [one,hundred,eight, ty,six]).
number_word(187, [one,hundred,eight, ty,seven]).
number_word(188, [one,hundred,eight, ty,eight]).
number_word(189, [one,hundred,eight, ty,nine]).
number_word(190, [one,hundred,nine, ty]).
number_word(191, [one,hundred,nine, ty,one]).
number_word(192, [one,hundred,nine, ty,two]).
number_word(193, [one,hundred,nine, ty,three]).
number_word(194, [one,hundred,nine, ty,four]).
number_word(195, [one,hundred,nine, ty,five]).
number_word(196, [one,hundred,nine, ty,six]).
number_word(197, [one,hundred,nine, ty,seven]).
number_word(198, [one,hundred,nine, ty,eight]).
number_word(199, [one,hundred,nine, ty,nine]).
number_word(200, [two,hundred]).
number_word(201, [two,hundred,one]).
number_word(202, [two,hundred,two]).
number_word(203, [two,hundred,three]).
number_word(204, [two,hundred,four]).
number_word(205, [two,hundred,five]).
number_word(206, [two,hundred,six]).
number_word(207, [two,hundred,seven]).
number_word(208, [two,hundred,eight]).
number_word(209, [two,hundred,nine]).
number_word(210, [two,hundred,ten]).
number_word(211, [two,hundred,eleven]).
number_word(212, [two,hundred,twelve]).
number_word(213, [two,hundred,thir, teen]).
number_word(214, [two,hundred,four, teen]).
number_word(215, [two,hundred,fif, teen]).
number_word(216, [two,hundred,six, teen]).
number_word(217, [two,hundred,seven, teen]).
number_word(218, [two,hundred,eight, teen]).
number_word(219, [two,hundred,nine, teen]).
number_word(220, [two,hundred,twen, ty]).
number_word(221, [two,hundred,twen, ty,one]).
number_word(222, [two,hundred,twen, ty,two]).
number_word(223, [two,hundred,twen, ty,three]).
number_word(224, [two,hundred,twen, ty,four]).
number_word(225, [two,hundred,twen, ty,five]).
number_word(226, [two,hundred,twen, ty,six]).
number_word(227, [two,hundred,twen, ty,seven]).
number_word(228, [two,hundred,twen, ty,eight]).
number_word(229, [two,hundred,twen, ty,nine]).
number_word(230, [two,hundred,thir, ty]).
number_word(231, [two,hundred,thir, ty,one]).
number_word(232, [two,hundred,thir, ty,two]).
number_word(233, [two,hundred,thir, ty,three]).
number_word(234, [two,hundred,thir, ty,four]).
number_word(235, [two,hundred,thir, ty,five]).
number_word(236, [two,hundred,thir, ty,six]).
number_word(237, [two,hundred,thir, ty,seven]).
number_word(238, [two,hundred,thir, ty,eight]).
number_word(239, [two,hundred,thir, ty,nine]).
number_word(240, [two,hundred,four, ty]).
number_word(241, [two,hundred,four, ty,one]).
number_word(242, [two,hundred,four, ty,two]).
number_word(243, [two,hundred,four, ty,three]).
number_word(244, [two,hundred,four, ty,four]).
number_word(245, [two,hundred,four, ty,five]).
number_word(246, [two,hundred,four, ty,six]).
number_word(247, [two,hundred,four, ty,seven]).
number_word(248, [two,hundred,four, ty,eight]).
number_word(249, [two,hundred,four, ty,nine]).
number_word(250, [two,hundred,fif, ty]).
number_word(251, [two,hundred,fif, ty,one]).
number_word(252, [two,hundred,fif, ty,two]).
number_word(253, [two,hundred,fif, ty,three]).
number_word(254, [two,hundred,fif, ty,four]).
number_word(255, [two,hundred,fif, ty,five]).
number_word(256, [two,hundred,fif, ty,six]).
number_word(257, [two,hundred,fif, ty,seven]).
number_word(258, [two,hundred,fif, ty,eight]).
number_word(259, [two,hundred,fif, ty,nine]).
number_word(260, [two,hundred,six, ty]).
number_word(261, [two,hundred,six, ty,one]).
number_word(262, [two,hundred,six, ty,two]).
number_word(263, [two,hundred,six, ty,three]).
number_word(264, [two,hundred,six, ty,four]).
number_word(265, [two,hundred,six, ty,five]).
number_word(266, [two,hundred,six, ty,six]).
number_word(267, [two,hundred,six, ty,seven]).
number_word(268, [two,hundred,six, ty,eight]).
number_word(269, [two,hundred,six, ty,nine]).
number_word(270, [two,hundred,seven, ty]).
number_word(271, [two,hundred,seven, ty,one]).
number_word(272, [two,hundred,seven, ty,two]).
number_word(273, [two,hundred,seven, ty,three]).
number_word(274, [two,hundred,seven, ty,four]).
number_word(275, [two,hundred,seven, ty,five]).
number_word(276, [two,hundred,seven, ty,six]).
number_word(277, [two,hundred,seven, ty,seven]).
number_word(278, [two,hundred,seven, ty,eight]).
number_word(279, [two,hundred,seven, ty,nine]).
number_word(280, [two,hundred,eight, ty]).
number_word(281, [two,hundred,eight, ty,one]).
number_word(282, [two,hundred,eight, ty,two]).
number_word(283, [two,hundred,eight, ty,three]).
number_word(284, [two,hundred,eight, ty,four]).
number_word(285, [two,hundred,eight, ty,five]).
number_word(286, [two,hundred,eight, ty,six]).
number_word(287, [two,hundred,eight, ty,seven]).
number_word(288, [two,hundred,eight, ty,eight]).
number_word(289, [two,hundred,eight, ty,nine]).
number_word(290, [two,hundred,nine, ty]).
number_word(291, [two,hundred,nine, ty,one]).
number_word(292, [two,hundred,nine, ty,two]).
number_word(293, [two,hundred,nine, ty,three]).
number_word(294, [two,hundred,nine, ty,four]).
number_word(295, [two,hundred,nine, ty,five]).
number_word(296, [two,hundred,nine, ty,six]).
number_word(297, [two,hundred,nine, ty,seven]).
number_word(298, [two,hundred,nine, ty,eight]).
number_word(299, [two,hundred,nine, ty,nine]).
number_word(300, [three,hundred]).
number_word(301, [three,hundred,one]).
number_word(302, [three,hundred,two]).
number_word(303, [three,hundred,three]).
number_word(304, [three,hundred,four]).
number_word(305, [three,hundred,five]).
number_word(306, [three,hundred,six]).
number_word(307, [three,hundred,seven]).
number_word(308, [three,hundred,eight]).
number_word(309, [three,hundred,nine]).
number_word(310, [three,hundred,ten]).
number_word(311, [three,hundred,eleven]).
number_word(312, [three,hundred,twelve]).
number_word(313, [three,hundred,thir, teen]).
number_word(314, [three,hundred,four, teen]).
number_word(315, [three,hundred,fif, teen]).
number_word(316, [three,hundred,six, teen]).
number_word(317, [three,hundred,seven, teen]).
number_word(318, [three,hundred,eight, teen]).
number_word(319, [three,hundred,nine, teen]).
number_word(320, [three,hundred,twen, ty]).
number_word(321, [three,hundred,twen, ty,one]).
number_word(322, [three,hundred,twen, ty,two]).
number_word(323, [three,hundred,twen, ty,three]).
number_word(324, [three,hundred,twen, ty,four]).
number_word(325, [three,hundred,twen, ty,five]).
number_word(326, [three,hundred,twen, ty,six]).
number_word(327, [three,hundred,twen, ty,seven]).
number_word(328, [three,hundred,twen, ty,eight]).
number_word(329, [three,hundred,twen, ty,nine]).
number_word(330, [three,hundred,thir, ty]).
number_word(331, [three,hundred,thir, ty,one]).
number_word(332, [three,hundred,thir, ty,two]).
number_word(333, [three,hundred,thir, ty,three]).
number_word(334, [three,hundred,thir, ty,four]).
number_word(335, [three,hundred,thir, ty,five]).
number_word(336, [three,hundred,thir, ty,six]).
number_word(337, [three,hundred,thir, ty,seven]).
number_word(338, [three,hundred,thir, ty,eight]).
number_word(339, [three,hundred,thir, ty,nine]).
number_word(340, [three,hundred,four, ty]).
number_word(341, [three,hundred,four, ty,one]).
number_word(342, [three,hundred,four, ty,two]).
number_word(343, [three,hundred,four, ty,three]).
number_word(344, [three,hundred,four, ty,four]).
number_word(345, [three,hundred,four, ty,five]).
number_word(346, [three,hundred,four, ty,six]).
number_word(347, [three,hundred,four, ty,seven]).
number_word(348, [three,hundred,four, ty,eight]).
number_word(349, [three,hundred,four, ty,nine]).
number_word(350, [three,hundred,fif, ty]).
number_word(351, [three,hundred,fif, ty,one]).
number_word(352, [three,hundred,fif, ty,two]).
number_word(353, [three,hundred,fif, ty,three]).
number_word(354, [three,hundred,fif, ty,four]).
number_word(355, [three,hundred,fif, ty,five]).
number_word(356, [three,hundred,fif, ty,six]).
number_word(357, [three,hundred,fif, ty,seven]).
number_word(358, [three,hundred,fif, ty,eight]).
number_word(359, [three,hundred,fif, ty,nine]).
number_word(360, [three,hundred,six, ty]).
number_word(361, [three,hundred,six, ty,one]).
number_word(362, [three,hundred,six, ty,two]).
number_word(363, [three,hundred,six, ty,three]).
number_word(364, [three,hundred,six, ty,four]).
number_word(365, [three,hundred,six, ty,five]).
number_word(366, [three,hundred,six, ty,six]).
number_word(367, [three,hundred,six, ty,seven]).
number_word(368, [three,hundred,six, ty,eight]).
number_word(369, [three,hundred,six, ty,nine]).
number_word(370, [three,hundred,seven, ty]).
number_word(371, [three,hundred,seven, ty,one]).
number_word(372, [three,hundred,seven, ty,two]).
number_word(373, [three,hundred,seven, ty,three]).
number_word(374, [three,hundred,seven, ty,four]).
number_word(375, [three,hundred,seven, ty,five]).
number_word(376, [three,hundred,seven, ty,six]).
number_word(377, [three,hundred,seven, ty,seven]).
number_word(378, [three,hundred,seven, ty,eight]).
number_word(379, [three,hundred,seven, ty,nine]).
number_word(380, [three,hundred,eight, ty]).
number_word(381, [three,hundred,eight, ty,one]).
number_word(382, [three,hundred,eight, ty,two]).
number_word(383, [three,hundred,eight, ty,three]).
number_word(384, [three,hundred,eight, ty,four]).
number_word(385, [three,hundred,eight, ty,five]).
number_word(386, [three,hundred,eight, ty,six]).
number_word(387, [three,hundred,eight, ty,seven]).
number_word(388, [three,hundred,eight, ty,eight]).
number_word(389, [three,hundred,eight, ty,nine]).
number_word(390, [three,hundred,nine, ty]).
number_word(391, [three,hundred,nine, ty,one]).
number_word(392, [three,hundred,nine, ty,two]).
number_word(393, [three,hundred,nine, ty,three]).
number_word(394, [three,hundred,nine, ty,four]).
number_word(395, [three,hundred,nine, ty,five]).
number_word(396, [three,hundred,nine, ty,six]).
number_word(397, [three,hundred,nine, ty,seven]).
number_word(398, [three,hundred,nine, ty,eight]).
number_word(399, [three,hundred,nine, ty,nine]).
number_word(400, [four,hundred]).
number_word(401, [four,hundred,one]).
number_word(402, [four,hundred,two]).
number_word(403, [four,hundred,three]).
number_word(404, [four,hundred,four]).
number_word(405, [four,hundred,five]).
number_word(406, [four,hundred,six]).
number_word(407, [four,hundred,seven]).
number_word(408, [four,hundred,eight]).
number_word(409, [four,hundred,nine]).
number_word(410, [four,hundred,ten]).
number_word(411, [four,hundred,eleven]).
number_word(412, [four,hundred,twelve]).
number_word(413, [four,hundred,thir, teen]).
number_word(414, [four,hundred,four, teen]).
number_word(415, [four,hundred,fif, teen]).
number_word(416, [four,hundred,six, teen]).
number_word(417, [four,hundred,seven, teen]).
number_word(418, [four,hundred,eight, teen]).
number_word(419, [four,hundred,nine, teen]).
number_word(420, [four,hundred,twen, ty]).
number_word(421, [four,hundred,twen, ty,one]).
number_word(422, [four,hundred,twen, ty,two]).
number_word(423, [four,hundred,twen, ty,three]).
number_word(424, [four,hundred,twen, ty,four]).
number_word(425, [four,hundred,twen, ty,five]).
number_word(426, [four,hundred,twen, ty,six]).
number_word(427, [four,hundred,twen, ty,seven]).
number_word(428, [four,hundred,twen, ty,eight]).
number_word(429, [four,hundred,twen, ty,nine]).
number_word(430, [four,hundred,thir, ty]).
number_word(431, [four,hundred,thir, ty,one]).
number_word(432, [four,hundred,thir, ty,two]).
number_word(433, [four,hundred,thir, ty,three]).
number_word(434, [four,hundred,thir, ty,four]).
number_word(435, [four,hundred,thir, ty,five]).
number_word(436, [four,hundred,thir, ty,six]).
number_word(437, [four,hundred,thir, ty,seven]).
number_word(438, [four,hundred,thir, ty,eight]).
number_word(439, [four,hundred,thir, ty,nine]).
number_word(440, [four,hundred,four, ty]).
number_word(441, [four,hundred,four, ty,one]).
number_word(442, [four,hundred,four, ty,two]).
number_word(443, [four,hundred,four, ty,three]).
number_word(444, [four,hundred,four, ty,four]).
number_word(445, [four,hundred,four, ty,five]).
number_word(446, [four,hundred,four, ty,six]).
number_word(447, [four,hundred,four, ty,seven]).
number_word(448, [four,hundred,four, ty,eight]).
number_word(449, [four,hundred,four, ty,nine]).
number_word(450, [four,hundred,fif, ty]).
number_word(451, [four,hundred,fif, ty,one]).
number_word(452, [four,hundred,fif, ty,two]).
number_word(453, [four,hundred,fif, ty,three]).
number_word(454, [four,hundred,fif, ty,four]).
number_word(455, [four,hundred,fif, ty,five]).
number_word(456, [four,hundred,fif, ty,six]).
number_word(457, [four,hundred,fif, ty,seven]).
number_word(458, [four,hundred,fif, ty,eight]).
number_word(459, [four,hundred,fif, ty,nine]).
number_word(460, [four,hundred,six, ty]).
number_word(461, [four,hundred,six, ty,one]).
number_word(462, [four,hundred,six, ty,two]).
number_word(463, [four,hundred,six, ty,three]).
number_word(464, [four,hundred,six, ty,four]).
number_word(465, [four,hundred,six, ty,five]).
number_word(466, [four,hundred,six, ty,six]).
number_word(467, [four,hundred,six, ty,seven]).
number_word(468, [four,hundred,six, ty,eight]).
number_word(469, [four,hundred,six, ty,nine]).
number_word(470, [four,hundred,seven, ty]).
number_word(471, [four,hundred,seven, ty,one]).
number_word(472, [four,hundred,seven, ty,two]).
number_word(473, [four,hundred,seven, ty,three]).
number_word(474, [four,hundred,seven, ty,four]).
number_word(475, [four,hundred,seven, ty,five]).
number_word(476, [four,hundred,seven, ty,six]).
number_word(477, [four,hundred,seven, ty,seven]).
number_word(478, [four,hundred,seven, ty,eight]).
number_word(479, [four,hundred,seven, ty,nine]).
number_word(480, [four,hundred,eight, ty]).
number_word(481, [four,hundred,eight, ty,one]).
number_word(482, [four,hundred,eight, ty,two]).
number_word(483, [four,hundred,eight, ty,three]).
number_word(484, [four,hundred,eight, ty,four]).
number_word(485, [four,hundred,eight, ty,five]).
number_word(486, [four,hundred,eight, ty,six]).
number_word(487, [four,hundred,eight, ty,seven]).
number_word(488, [four,hundred,eight, ty,eight]).
number_word(489, [four,hundred,eight, ty,nine]).
number_word(490, [four,hundred,nine, ty]).
number_word(491, [four,hundred,nine, ty,one]).
number_word(492, [four,hundred,nine, ty,two]).
number_word(493, [four,hundred,nine, ty,three]).
number_word(494, [four,hundred,nine, ty,four]).
number_word(495, [four,hundred,nine, ty,five]).
number_word(496, [four,hundred,nine, ty,six]).
number_word(497, [four,hundred,nine, ty,seven]).
number_word(498, [four,hundred,nine, ty,eight]).
number_word(499, [four,hundred,nine, ty,nine]).
number_word(500, [five,hundred]).
number_word(501, [five,hundred,one]).
number_word(502, [five,hundred,two]).
number_word(503, [five,hundred,three]).
number_word(504, [five,hundred,four]).
number_word(505, [five,hundred,five]).
number_word(506, [five,hundred,six]).
number_word(507, [five,hundred,seven]).
number_word(508, [five,hundred,eight]).
number_word(509, [five,hundred,nine]).
number_word(510, [five,hundred,ten]).
number_word(511, [five,hundred,eleven]).
number_word(512, [five,hundred,twelve]).
number_word(513, [five,hundred,thir, teen]).
number_word(514, [five,hundred,four, teen]).
number_word(515, [five,hundred,fif, teen]).
number_word(516, [five,hundred,six, teen]).
number_word(517, [five,hundred,seven, teen]).
number_word(518, [five,hundred,eight, teen]).
number_word(519, [five,hundred,nine, teen]).
number_word(520, [five,hundred,twen, ty]).
number_word(521, [five,hundred,twen, ty,one]).
number_word(522, [five,hundred,twen, ty,two]).
number_word(523, [five,hundred,twen, ty,three]).
number_word(524, [five,hundred,twen, ty,four]).
number_word(525, [five,hundred,twen, ty,five]).
number_word(526, [five,hundred,twen, ty,six]).
number_word(527, [five,hundred,twen, ty,seven]).
number_word(528, [five,hundred,twen, ty,eight]).
number_word(529, [five,hundred,twen, ty,nine]).
number_word(530, [five,hundred,thir, ty]).
number_word(531, [five,hundred,thir, ty,one]).
number_word(532, [five,hundred,thir, ty,two]).
number_word(533, [five,hundred,thir, ty,three]).
number_word(534, [five,hundred,thir, ty,four]).
number_word(535, [five,hundred,thir, ty,five]).
number_word(536, [five,hundred,thir, ty,six]).
number_word(537, [five,hundred,thir, ty,seven]).
number_word(538, [five,hundred,thir, ty,eight]).
number_word(539, [five,hundred,thir, ty,nine]).
number_word(540, [five,hundred,four, ty]).
number_word(541, [five,hundred,four, ty,one]).
number_word(542, [five,hundred,four, ty,two]).
number_word(543, [five,hundred,four, ty,three]).
number_word(544, [five,hundred,four, ty,four]).
number_word(545, [five,hundred,four, ty,five]).
number_word(546, [five,hundred,four, ty,six]).
number_word(547, [five,hundred,four, ty,seven]).
number_word(548, [five,hundred,four, ty,eight]).
number_word(549, [five,hundred,four, ty,nine]).
number_word(550, [five,hundred,fif, ty]).
number_word(551, [five,hundred,fif, ty,one]).
number_word(552, [five,hundred,fif, ty,two]).
number_word(553, [five,hundred,fif, ty,three]).
number_word(554, [five,hundred,fif, ty,four]).
number_word(555, [five,hundred,fif, ty,five]).
number_word(556, [five,hundred,fif, ty,six]).
number_word(557, [five,hundred,fif, ty,seven]).
number_word(558, [five,hundred,fif, ty,eight]).
number_word(559, [five,hundred,fif, ty,nine]).
number_word(560, [five,hundred,six, ty]).
number_word(561, [five,hundred,six, ty,one]).
number_word(562, [five,hundred,six, ty,two]).
number_word(563, [five,hundred,six, ty,three]).
number_word(564, [five,hundred,six, ty,four]).
number_word(565, [five,hundred,six, ty,five]).
number_word(566, [five,hundred,six, ty,six]).
number_word(567, [five,hundred,six, ty,seven]).
number_word(568, [five,hundred,six, ty,eight]).
number_word(569, [five,hundred,six, ty,nine]).
number_word(570, [five,hundred,seven, ty]).
number_word(571, [five,hundred,seven, ty,one]).
number_word(572, [five,hundred,seven, ty,two]).
number_word(573, [five,hundred,seven, ty,three]).
number_word(574, [five,hundred,seven, ty,four]).
number_word(575, [five,hundred,seven, ty,five]).
number_word(576, [five,hundred,seven, ty,six]).
number_word(577, [five,hundred,seven, ty,seven]).
number_word(578, [five,hundred,seven, ty,eight]).
number_word(579, [five,hundred,seven, ty,nine]).
number_word(580, [five,hundred,eight, ty]).
number_word(581, [five,hundred,eight, ty,one]).
number_word(582, [five,hundred,eight, ty,two]).
number_word(583, [five,hundred,eight, ty,three]).
number_word(584, [five,hundred,eight, ty,four]).
number_word(585, [five,hundred,eight, ty,five]).
number_word(586, [five,hundred,eight, ty,six]).
number_word(587, [five,hundred,eight, ty,seven]).
number_word(588, [five,hundred,eight, ty,eight]).
number_word(589, [five,hundred,eight, ty,nine]).
number_word(590, [five,hundred,nine, ty]).
number_word(591, [five,hundred,nine, ty,one]).
number_word(592, [five,hundred,nine, ty,two]).
number_word(593, [five,hundred,nine, ty,three]).
number_word(594, [five,hundred,nine, ty,four]).
number_word(595, [five,hundred,nine, ty,five]).
number_word(596, [five,hundred,nine, ty,six]).
number_word(597, [five,hundred,nine, ty,seven]).
number_word(598, [five,hundred,nine, ty,eight]).
number_word(599, [five,hundred,nine, ty,nine]).
number_word(600, [six,hundred]).
number_word(601, [six,hundred,one]).
number_word(602, [six,hundred,two]).
number_word(603, [six,hundred,three]).
number_word(604, [six,hundred,four]).
number_word(605, [six,hundred,five]).
number_word(606, [six,hundred,six]).
number_word(607, [six,hundred,seven]).
number_word(608, [six,hundred,eight]).
number_word(609, [six,hundred,nine]).
number_word(610, [six,hundred,ten]).
number_word(611, [six,hundred,eleven]).
number_word(612, [six,hundred,twelve]).
number_word(613, [six,hundred,thir, teen]).
number_word(614, [six,hundred,four, teen]).
number_word(615, [six,hundred,fif, teen]).
number_word(616, [six,hundred,six, teen]).
number_word(617, [six,hundred,seven, teen]).
number_word(618, [six,hundred,eight, teen]).
number_word(619, [six,hundred,nine, teen]).
number_word(620, [six,hundred,twen, ty]).
number_word(621, [six,hundred,twen, ty,one]).
number_word(622, [six,hundred,twen, ty,two]).
number_word(623, [six,hundred,twen, ty,three]).
number_word(624, [six,hundred,twen, ty,four]).
number_word(625, [six,hundred,twen, ty,five]).
number_word(626, [six,hundred,twen, ty,six]).
number_word(627, [six,hundred,twen, ty,seven]).
number_word(628, [six,hundred,twen, ty,eight]).
number_word(629, [six,hundred,twen, ty,nine]).
number_word(630, [six,hundred,thir, ty]).
number_word(631, [six,hundred,thir, ty,one]).
number_word(632, [six,hundred,thir, ty,two]).
number_word(633, [six,hundred,thir, ty,three]).
number_word(634, [six,hundred,thir, ty,four]).
number_word(635, [six,hundred,thir, ty,five]).
number_word(636, [six,hundred,thir, ty,six]).
number_word(637, [six,hundred,thir, ty,seven]).
number_word(638, [six,hundred,thir, ty,eight]).
number_word(639, [six,hundred,thir, ty,nine]).
number_word(640, [six,hundred,four, ty]).
number_word(641, [six,hundred,four, ty,one]).
number_word(642, [six,hundred,four, ty,two]).
number_word(643, [six,hundred,four, ty,three]).
number_word(644, [six,hundred,four, ty,four]).
number_word(645, [six,hundred,four, ty,five]).
number_word(646, [six,hundred,four, ty,six]).
number_word(647, [six,hundred,four, ty,seven]).
number_word(648, [six,hundred,four, ty,eight]).
number_word(649, [six,hundred,four, ty,nine]).
number_word(650, [six,hundred,fif, ty]).
number_word(651, [six,hundred,fif, ty,one]).
number_word(652, [six,hundred,fif, ty,two]).
number_word(653, [six,hundred,fif, ty,three]).
number_word(654, [six,hundred,fif, ty,four]).
number_word(655, [six,hundred,fif, ty,five]).
number_word(656, [six,hundred,fif, ty,six]).
number_word(657, [six,hundred,fif, ty,seven]).
number_word(658, [six,hundred,fif, ty,eight]).
number_word(659, [six,hundred,fif, ty,nine]).
number_word(660, [six,hundred,six, ty]).
number_word(661, [six,hundred,six, ty,one]).
number_word(662, [six,hundred,six, ty,two]).
number_word(663, [six,hundred,six, ty,three]).
number_word(664, [six,hundred,six, ty,four]).
number_word(665, [six,hundred,six, ty,five]).
number_word(666, [six,hundred,six, ty,six]).
number_word(667, [six,hundred,six, ty,seven]).
number_word(668, [six,hundred,six, ty,eight]).
number_word(669, [six,hundred,six, ty,nine]).
number_word(670, [six,hundred,seven, ty]).
number_word(671, [six,hundred,seven, ty,one]).
number_word(672, [six,hundred,seven, ty,two]).
number_word(673, [six,hundred,seven, ty,three]).
number_word(674, [six,hundred,seven, ty,four]).
number_word(675, [six,hundred,seven, ty,five]).
number_word(676, [six,hundred,seven, ty,six]).
number_word(677, [six,hundred,seven, ty,seven]).
number_word(678, [six,hundred,seven, ty,eight]).
number_word(679, [six,hundred,seven, ty,nine]).
number_word(680, [six,hundred,eight, ty]).
number_word(681, [six,hundred,eight, ty,one]).
number_word(682, [six,hundred,eight, ty,two]).
number_word(683, [six,hundred,eight, ty,three]).
number_word(684, [six,hundred,eight, ty,four]).
number_word(685, [six,hundred,eight, ty,five]).
number_word(686, [six,hundred,eight, ty,six]).
number_word(687, [six,hundred,eight, ty,seven]).
number_word(688, [six,hundred,eight, ty,eight]).
number_word(689, [six,hundred,eight, ty,nine]).
number_word(690, [six,hundred,nine, ty]).
number_word(691, [six,hundred,nine, ty,one]).
number_word(692, [six,hundred,nine, ty,two]).
number_word(693, [six,hundred,nine, ty,three]).
number_word(694, [six,hundred,nine, ty,four]).
number_word(695, [six,hundred,nine, ty,five]).
number_word(696, [six,hundred,nine, ty,six]).
number_word(697, [six,hundred,nine, ty,seven]).
number_word(698, [six,hundred,nine, ty,eight]).
number_word(699, [six,hundred,nine, ty,nine]).
number_word(700, [seven,hundred]).
number_word(701, [seven,hundred,one]).
number_word(702, [seven,hundred,two]).
number_word(703, [seven,hundred,three]).
number_word(704, [seven,hundred,four]).
number_word(705, [seven,hundred,five]).
number_word(706, [seven,hundred,six]).
number_word(707, [seven,hundred,seven]).
number_word(708, [seven,hundred,eight]).
number_word(709, [seven,hundred,nine]).
number_word(710, [seven,hundred,ten]).
number_word(711, [seven,hundred,eleven]).
number_word(712, [seven,hundred,twelve]).
number_word(713, [seven,hundred,thir, teen]).
number_word(714, [seven,hundred,four, teen]).
number_word(715, [seven,hundred,fif, teen]).
number_word(716, [seven,hundred,six, teen]).
number_word(717, [seven,hundred,seven, teen]).
number_word(718, [seven,hundred,eight, teen]).
number_word(719, [seven,hundred,nine, teen]).
number_word(720, [seven,hundred,twen, ty]).
number_word(721, [seven,hundred,twen, ty,one]).
number_word(722, [seven,hundred,twen, ty,two]).
number_word(723, [seven,hundred,twen, ty,three]).
number_word(724, [seven,hundred,twen, ty,four]).
number_word(725, [seven,hundred,twen, ty,five]).
number_word(726, [seven,hundred,twen, ty,six]).
number_word(727, [seven,hundred,twen, ty,seven]).
number_word(728, [seven,hundred,twen, ty,eight]).
number_word(729, [seven,hundred,twen, ty,nine]).
number_word(730, [seven,hundred,thir, ty]).
number_word(731, [seven,hundred,thir, ty,one]).
number_word(732, [seven,hundred,thir, ty,two]).
number_word(733, [seven,hundred,thir, ty,three]).
number_word(734, [seven,hundred,thir, ty,four]).
number_word(735, [seven,hundred,thir, ty,five]).
number_word(736, [seven,hundred,thir, ty,six]).
number_word(737, [seven,hundred,thir, ty,seven]).
number_word(738, [seven,hundred,thir, ty,eight]).
number_word(739, [seven,hundred,thir, ty,nine]).
number_word(740, [seven,hundred,four, ty]).
number_word(741, [seven,hundred,four, ty,one]).
number_word(742, [seven,hundred,four, ty,two]).
number_word(743, [seven,hundred,four, ty,three]).
number_word(744, [seven,hundred,four, ty,four]).
number_word(745, [seven,hundred,four, ty,five]).
number_word(746, [seven,hundred,four, ty,six]).
number_word(747, [seven,hundred,four, ty,seven]).
number_word(748, [seven,hundred,four, ty,eight]).
number_word(749, [seven,hundred,four, ty,nine]).
number_word(750, [seven,hundred,fif, ty]).
number_word(751, [seven,hundred,fif, ty,one]).
number_word(752, [seven,hundred,fif, ty,two]).
number_word(753, [seven,hundred,fif, ty,three]).
number_word(754, [seven,hundred,fif, ty,four]).
number_word(755, [seven,hundred,fif, ty,five]).
number_word(756, [seven,hundred,fif, ty,six]).
number_word(757, [seven,hundred,fif, ty,seven]).
number_word(758, [seven,hundred,fif, ty,eight]).
number_word(759, [seven,hundred,fif, ty,nine]).
number_word(760, [seven,hundred,six, ty]).
number_word(761, [seven,hundred,six, ty,one]).
number_word(762, [seven,hundred,six, ty,two]).
number_word(763, [seven,hundred,six, ty,three]).
number_word(764, [seven,hundred,six, ty,four]).
number_word(765, [seven,hundred,six, ty,five]).
number_word(766, [seven,hundred,six, ty,six]).
number_word(767, [seven,hundred,six, ty,seven]).
number_word(768, [seven,hundred,six, ty,eight]).
number_word(769, [seven,hundred,six, ty,nine]).
number_word(770, [seven,hundred,seven, ty]).
number_word(771, [seven,hundred,seven, ty,one]).
number_word(772, [seven,hundred,seven, ty,two]).
number_word(773, [seven,hundred,seven, ty,three]).
number_word(774, [seven,hundred,seven, ty,four]).
number_word(775, [seven,hundred,seven, ty,five]).
number_word(776, [seven,hundred,seven, ty,six]).
number_word(777, [seven,hundred,seven, ty,seven]).
number_word(778, [seven,hundred,seven, ty,eight]).
number_word(779, [seven,hundred,seven, ty,nine]).
number_word(780, [seven,hundred,eight, ty]).
number_word(781, [seven,hundred,eight, ty,one]).
number_word(782, [seven,hundred,eight, ty,two]).
number_word(783, [seven,hundred,eight, ty,three]).
number_word(784, [seven,hundred,eight, ty,four]).
number_word(785, [seven,hundred,eight, ty,five]).
number_word(786, [seven,hundred,eight, ty,six]).
number_word(787, [seven,hundred,eight, ty,seven]).
number_word(788, [seven,hundred,eight, ty,eight]).
number_word(789, [seven,hundred,eight, ty,nine]).
number_word(790, [seven,hundred,nine, ty]).
number_word(791, [seven,hundred,nine, ty,one]).
number_word(792, [seven,hundred,nine, ty,two]).
number_word(793, [seven,hundred,nine, ty,three]).
number_word(794, [seven,hundred,nine, ty,four]).
number_word(795, [seven,hundred,nine, ty,five]).
number_word(796, [seven,hundred,nine, ty,six]).
number_word(797, [seven,hundred,nine, ty,seven]).
number_word(798, [seven,hundred,nine, ty,eight]).
number_word(799, [seven,hundred,nine, ty,nine]).
number_word(800, [eight,hundred]).
number_word(801, [eight,hundred,one]).
number_word(802, [eight,hundred,two]).
number_word(803, [eight,hundred,three]).
number_word(804, [eight,hundred,four]).
number_word(805, [eight,hundred,five]).
number_word(806, [eight,hundred,six]).
number_word(807, [eight,hundred,seven]).
number_word(808, [eight,hundred,eight]).
number_word(809, [eight,hundred,nine]).
number_word(810, [eight,hundred,ten]).
number_word(811, [eight,hundred,eleven]).
number_word(812, [eight,hundred,twelve]).
number_word(813, [eight,hundred,thir, teen]).
number_word(814, [eight,hundred,four, teen]).
number_word(815, [eight,hundred,fif, teen]).
number_word(816, [eight,hundred,six, teen]).
number_word(817, [eight,hundred,seven, teen]).
number_word(818, [eight,hundred,eight, teen]).
number_word(819, [eight,hundred,nine, teen]).
number_word(820, [eight,hundred,twen, ty]).
number_word(821, [eight,hundred,twen, ty,one]).
number_word(822, [eight,hundred,twen, ty,two]).
number_word(823, [eight,hundred,twen, ty,three]).
number_word(824, [eight,hundred,twen, ty,four]).
number_word(825, [eight,hundred,twen, ty,five]).
number_word(826, [eight,hundred,twen, ty,six]).
number_word(827, [eight,hundred,twen, ty,seven]).
number_word(828, [eight,hundred,twen, ty,eight]).
number_word(829, [eight,hundred,twen, ty,nine]).
number_word(830, [eight,hundred,thir, ty]).
number_word(831, [eight,hundred,thir, ty,one]).
number_word(832, [eight,hundred,thir, ty,two]).
number_word(833, [eight,hundred,thir, ty,three]).
number_word(834, [eight,hundred,thir, ty,four]).
number_word(835, [eight,hundred,thir, ty,five]).
number_word(836, [eight,hundred,thir, ty,six]).
number_word(837, [eight,hundred,thir, ty,seven]).
number_word(838, [eight,hundred,thir, ty,eight]).
number_word(839, [eight,hundred,thir, ty,nine]).
number_word(840, [eight,hundred,four, ty]).
number_word(841, [eight,hundred,four, ty,one]).
number_word(842, [eight,hundred,four, ty,two]).
number_word(843, [eight,hundred,four, ty,three]).
number_word(844, [eight,hundred,four, ty,four]).
number_word(845, [eight,hundred,four, ty,five]).
number_word(846, [eight,hundred,four, ty,six]).
number_word(847, [eight,hundred,four, ty,seven]).
number_word(848, [eight,hundred,four, ty,eight]).
number_word(849, [eight,hundred,four, ty,nine]).
number_word(850, [eight,hundred,fif, ty]).
number_word(851, [eight,hundred,fif, ty,one]).
number_word(852, [eight,hundred,fif, ty,two]).
number_word(853, [eight,hundred,fif, ty,three]).
number_word(854, [eight,hundred,fif, ty,four]).
number_word(855, [eight,hundred,fif, ty,five]).
number_word(856, [eight,hundred,fif, ty,six]).
number_word(857, [eight,hundred,fif, ty,seven]).
number_word(858, [eight,hundred,fif, ty,eight]).
number_word(859, [eight,hundred,fif, ty,nine]).
number_word(860, [eight,hundred,six, ty]).
number_word(861, [eight,hundred,six, ty,one]).
number_word(862, [eight,hundred,six, ty,two]).
number_word(863, [eight,hundred,six, ty,three]).
number_word(864, [eight,hundred,six, ty,four]).
number_word(865, [eight,hundred,six, ty,five]).
number_word(866, [eight,hundred,six, ty,six]).
number_word(867, [eight,hundred,six, ty,seven]).
number_word(868, [eight,hundred,six, ty,eight]).
number_word(869, [eight,hundred,six, ty,nine]).
number_word(870, [eight,hundred,seven, ty]).
number_word(871, [eight,hundred,seven, ty,one]).
number_word(872, [eight,hundred,seven, ty,two]).
number_word(873, [eight,hundred,seven, ty,three]).
number_word(874, [eight,hundred,seven, ty,four]).
number_word(875, [eight,hundred,seven, ty,five]).
number_word(876, [eight,hundred,seven, ty,six]).
number_word(877, [eight,hundred,seven, ty,seven]).
number_word(878, [eight,hundred,seven, ty,eight]).
number_word(879, [eight,hundred,seven, ty,nine]).
number_word(880, [eight,hundred,eight, ty]).
number_word(881, [eight,hundred,eight, ty,one]).
number_word(882, [eight,hundred,eight, ty,two]).
number_word(883, [eight,hundred,eight, ty,three]).
number_word(884, [eight,hundred,eight, ty,four]).
number_word(885, [eight,hundred,eight, ty,five]).
number_word(886, [eight,hundred,eight, ty,six]).
number_word(887, [eight,hundred,eight, ty,seven]).
number_word(888, [eight,hundred,eight, ty,eight]).
number_word(889, [eight,hundred,eight, ty,nine]).
number_word(890, [eight,hundred,nine, ty]).
number_word(891, [eight,hundred,nine, ty,one]).
number_word(892, [eight,hundred,nine, ty,two]).
number_word(893, [eight,hundred,nine, ty,three]).
number_word(894, [eight,hundred,nine, ty,four]).
number_word(895, [eight,hundred,nine, ty,five]).
number_word(896, [eight,hundred,nine, ty,six]).
number_word(897, [eight,hundred,nine, ty,seven]).
number_word(898, [eight,hundred,nine, ty,eight]).
number_word(899, [eight,hundred,nine, ty,nine]).
number_word(900, [nine,hundred]).
number_word(901, [nine,hundred,one]).
number_word(902, [nine,hundred,two]).
number_word(903, [nine,hundred,three]).
number_word(904, [nine,hundred,four]).
number_word(905, [nine,hundred,five]).
number_word(906, [nine,hundred,six]).
number_word(907, [nine,hundred,seven]).
number_word(908, [nine,hundred,eight]).
number_word(909, [nine,hundred,nine]).
number_word(910, [nine,hundred,ten]).
number_word(911, [nine,hundred,eleven]).
number_word(912, [nine,hundred,twelve]).
number_word(913, [nine,hundred,thir, teen]).
number_word(914, [nine,hundred,four, teen]).
number_word(915, [nine,hundred,fif, teen]).
number_word(916, [nine,hundred,six, teen]).
number_word(917, [nine,hundred,seven, teen]).
number_word(918, [nine,hundred,eight, teen]).
number_word(919, [nine,hundred,nine, teen]).
number_word(920, [nine,hundred,twen, ty]).
number_word(921, [nine,hundred,twen, ty,one]).
number_word(922, [nine,hundred,twen, ty,two]).
number_word(923, [nine,hundred,twen, ty,three]).
number_word(924, [nine,hundred,twen, ty,four]).
number_word(925, [nine,hundred,twen, ty,five]).
number_word(926, [nine,hundred,twen, ty,six]).
number_word(927, [nine,hundred,twen, ty,seven]).
number_word(928, [nine,hundred,twen, ty,eight]).
number_word(929, [nine,hundred,twen, ty,nine]).
number_word(930, [nine,hundred,thir, ty]).
number_word(931, [nine,hundred,thir, ty,one]).
number_word(932, [nine,hundred,thir, ty,two]).
number_word(933, [nine,hundred,thir, ty,three]).
number_word(934, [nine,hundred,thir, ty,four]).
number_word(935, [nine,hundred,thir, ty,five]).
number_word(936, [nine,hundred,thir, ty,six]).
number_word(937, [nine,hundred,thir, ty,seven]).
number_word(938, [nine,hundred,thir, ty,eight]).
number_word(939, [nine,hundred,thir, ty,nine]).
number_word(940, [nine,hundred,four, ty]).
number_word(941, [nine,hundred,four, ty,one]).
number_word(942, [nine,hundred,four, ty,two]).
number_word(943, [nine,hundred,four, ty,three]).
number_word(944, [nine,hundred,four, ty,four]).
number_word(945, [nine,hundred,four, ty,five]).
number_word(946, [nine,hundred,four, ty,six]).
number_word(947, [nine,hundred,four, ty,seven]).
number_word(948, [nine,hundred,four, ty,eight]).
number_word(949, [nine,hundred,four, ty,nine]).
number_word(950, [nine,hundred,fif, ty]).
number_word(951, [nine,hundred,fif, ty,one]).
number_word(952, [nine,hundred,fif, ty,two]).
number_word(953, [nine,hundred,fif, ty,three]).
number_word(954, [nine,hundred,fif, ty,four]).
number_word(955, [nine,hundred,fif, ty,five]).
number_word(956, [nine,hundred,fif, ty,six]).
number_word(957, [nine,hundred,fif, ty,seven]).
number_word(958, [nine,hundred,fif, ty,eight]).
number_word(959, [nine,hundred,fif, ty,nine]).
number_word(960, [nine,hundred,six, ty]).
number_word(961, [nine,hundred,six, ty,one]).
number_word(962, [nine,hundred,six, ty,two]).
number_word(963, [nine,hundred,six, ty,three]).
number_word(964, [nine,hundred,six, ty,four]).
number_word(965, [nine,hundred,six, ty,five]).
number_word(966, [nine,hundred,six, ty,six]).
number_word(967, [nine,hundred,six, ty,seven]).
number_word(968, [nine,hundred,six, ty,eight]).
number_word(969, [nine,hundred,six, ty,nine]).
number_word(970, [nine,hundred,seven, ty]).
number_word(971, [nine,hundred,seven, ty,one]).
number_word(972, [nine,hundred,seven, ty,two]).
number_word(973, [nine,hundred,seven, ty,three]).
number_word(974, [nine,hundred,seven, ty,four]).
number_word(975, [nine,hundred,seven, ty,five]).
number_word(976, [nine,hundred,seven, ty,six]).
number_word(977, [nine,hundred,seven, ty,seven]).
number_word(978, [nine,hundred,seven, ty,eight]).
number_word(979, [nine,hundred,seven, ty,nine]).
number_word(980, [nine,hundred,eight, ty]).
number_word(981, [nine,hundred,eight, ty,one]).
number_word(982, [nine,hundred,eight, ty,two]).
number_word(983, [nine,hundred,eight, ty,three]).
number_word(984, [nine,hundred,eight, ty,four]).
number_word(985, [nine,hundred,eight, ty,five]).
number_word(986, [nine,hundred,eight, ty,six]).
number_word(987, [nine,hundred,eight, ty,seven]).
number_word(988, [nine,hundred,eight, ty,eight]).
number_word(989, [nine,hundred,eight, ty,nine]).
number_word(990, [nine,hundred,nine, ty]).
number_word(991, [nine,hundred,nine, ty,one]).
number_word(992, [nine,hundred,nine, ty,two]).
number_word(993, [nine,hundred,nine, ty,three]).
number_word(994, [nine,hundred,nine, ty,four]).
number_word(995, [nine,hundred,nine, ty,five]).
number_word(996, [nine,hundred,nine, ty,six]).
number_word(997, [nine,hundred,nine, ty,seven]).
number_word(998, [nine,hundred,nine, ty,eight]).
number_word(999, [nine,hundred,nine, ty,nine]).
number_word(1000, [one,thousand]).

        
        
    
    
    
